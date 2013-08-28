{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Narrative.SrcParser where

import Text.Parsec hiding ((<|>), optional)
import Control.Applicative
import Control.Monad.Reader
import Data.Monoid
import Data.String
import Test.QuickCheck.All

data Chunk s =
    Comment [(s, s)]
  | Code [s]
  | Ignore [(s, s)]

data ParserOpts = ParserOpts
  { commentToken :: String
  , ignoreToken :: String
  }

type POR = Reader ParserOpts
type Parser s u = ParsecT s u POR

pprintChunk :: (Stream s POR Char, Monoid s, IsString s) => Chunk s -> POR s
pprintChunk c = do
  r <- ask
  return $ case c of
    Comment x -> (mconcat . map (\(a, b) -> a <> fromString (commentToken r) <> b)) x
    Code x    -> mconcat x
    Ignore x  -> (mconcat . map (\(a, b) -> a <> fromString (ignoreToken r) <> b)) x

nlOrEof :: Stream s m Char => ParsecT s u m ()
nlOrEof = try newline *> pure () <|> eof

manyCharTill :: (Stream s m t, IsString s) => ParsecT s u m Char -> ParsecT s u m end -> ParsecT s u m s
manyCharTill p end = fromString <$> manyTill p end

nl :: (Stream s m Char, Monoid s, IsString s) => ParsecT s u m s
nl = newline *> pure "\n"

linePrefix :: Stream s m Char => String -> ParsecT s u m ()
linePrefix s = string s *> pure ()

prefixedLine :: (Stream s m Char, Monoid s, IsString s) => String -> ParsecT s u m (s, s)
prefixedLine s = -- map (fromString *** fromString) $
  (,)
  <$> (optional (many1 space) <* linePrefix s *> pure "")
  <*> (mappend <$> manyCharTill anyChar nlOrEof <*> nl)

comment :: (Stream s POR Char, Monoid s, IsString s) => Parser s u (Chunk s)
comment = do
  s <- asks commentToken
  Comment <$> many1 (prefixedLine s)

ignore :: (Stream s POR Char, Monoid s, IsString s) => Parser s u (Chunk s)
ignore = do
  s <- asks ignoreToken
  Ignore <$> many1 (prefixedLine s)

notCode :: (Stream s POR Char, Monoid s, IsString s) => Parser s u (Chunk s)
notCode = try comment <|> ignore

code :: (Stream s POR Char, Monoid s, IsString s) => Parser s u (Chunk s)
code = fmap Code $ manyTill (manyCharTill anyChar nlOrEof) $ try notCode *> pure () <|> try eof

document :: (Stream s POR Char, Monoid s, IsString s) => Parser s u [(Chunk s)]
document = manyTill (try notCode <|> code) eof

chunks :: (Stream s POR Char, Monoid s, IsString s) => ParserOpts -> SourceName -> s -> Either ParseError [(Chunk s)]
chunks os n s = runReader (runParserT document () n s) os

checkAll = $quickCheckAll
