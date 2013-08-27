{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Narrative.SrcParser where

import Text.Parsec
import Control.Applicative ((<$>), (<*>), (*>), pure)
import Control.Monad (join, liftM)
import Control.Monad.Reader (Reader, asks, ask, runReader)
import Control.Monad.Trans.Class (lift)

data Chunk = Comment String
           | Code String
           | Ignore String

data ParserOpts = ParserOpts
  { commentToken :: String
  , ignoreToken :: String
  }

type POR = Reader ParserOpts
type Parser s u = ParsecT s u POR

nlOrEof :: Stream s m Char => ParsecT s u m ()
nlOrEof = try newline *> pure () <|> eof

linePrefix :: Stream s m Char => String -> ParsecT s u m ()
linePrefix s = string s *> optional space *> pure ()

prefixedLine :: Stream s m Char => String -> ParsecT s u m String
prefixedLine s = spaces *> linePrefix s *> manyTill anyChar (try nlOrEof)

comment :: Stream s POR Char => Parser s u Chunk
comment = do
  s <- asks commentToken
  fmap (Comment . join) $ many1 $ prefixedLine s

ignore :: Stream s POR Char => Parser s u Chunk
ignore = do
  s <- asks ignoreToken
  fmap (Ignore . join) $ many1 $ prefixedLine s

notCode :: Stream s POR Char => Parser s u Chunk
notCode = try comment <|> ignore

code :: Stream s POR Char => Parser s u Chunk
code = fmap Code $ manyTill anyChar $ try notCode *> pure () <|> try eof

document :: Stream s POR Char => Parser s u [Chunk]
document = manyTill (try notCode <|> code) eof

chunks :: Stream s POR Char => ParserOpts -> SourceName -> s -> Either ParseError [Chunk]
chunks os n s = runReader (runParserT document () n s) os
