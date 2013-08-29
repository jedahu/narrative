module Narrative.SrcParser where

import Text.Parsec hiding ((<|>), optional)
import Control.Applicative
import Control.Monad.Reader
import Data.Monoid
import Data.String
import Narrative.CharSeq

data Chunk s =
    Comment [(s, s)]
  | Code [s]
  | Ignore [(s, s)]

data ParserOpts s = ParserOpts
  { commentToken :: s
  , ignoreToken :: s
  }

type POR s = Reader (ParserOpts s)
type Parser s u = ParsecT s u (POR s)

pprintChunk
  :: (Stream s (POR s) Char, Monoid s)
  => Chunk s -> POR s s
pprintChunk c = do
  r <- ask
  return $ case c of
    Comment x -> (mconcat . map (\(a, b) -> a <> commentToken r <> b)) x
    Code x    -> mconcat x
    Ignore x  -> (mconcat . map (\(a, b) -> a <> ignoreToken r <> b)) x

nlOrEof :: Stream s m Char => ParsecT s u m ()
nlOrEof = try newline *> pure () <|> eof

manyCharTill
  :: (Stream s m t, CharSeq s)
  => ParsecT s u m Char -> ParsecT s u m end -> ParsecT s u m s
manyCharTill p end = fromString <$> manyTill p end

nl :: (Stream s m Char, Monoid s, CharSeq s) => ParsecT s u m s
nl = newline *> pure (fromString "\n")

stream :: (Stream s m Char, CharSeq s) => s -> ParsecT s u m s
stream s = fromString <$> string (toString s)

linePrefix :: (Stream s m Char, CharSeq s) => s -> ParsecT s u m ()
linePrefix s = stream s *> pure ()

prefixedLine
  :: (Stream s m Char, Monoid s, CharSeq s)
  => s -> ParsecT s u m (s, s)
prefixedLine s = -- map (fromString *** fromString) $
  (,)
  <$> (optional (many1 space) <* linePrefix s *> pure (fromString ""))
  <*> (mappend <$> manyCharTill anyChar nlOrEof <*> nl)

comment
  :: (Stream s (POR s) Char, Monoid s, CharSeq s)
  => Parser s u (Chunk s)
comment = do
  s <- asks commentToken
  Comment <$> many1 (prefixedLine s)

ignore
  :: (Stream s (POR s) Char, Monoid s, CharSeq s)
  => Parser s u (Chunk s)
ignore = do
  s <- asks ignoreToken
  Ignore <$> many1 (prefixedLine s)

notCode
  :: (Stream s (POR s) Char, Monoid s, CharSeq s)
  => Parser s u (Chunk s)
notCode = try comment <|> ignore

code
  :: (Stream s (POR s) Char, Monoid s, CharSeq s)
  => Parser s u (Chunk s)
code = fmap Code
  $ manyTill (manyCharTill anyChar nlOrEof)
  $ try notCode *> pure () <|> try eof

document
  :: (Stream s (POR s) Char, Monoid s, CharSeq s)
  => Parser s u [(Chunk s)]
document = manyTill (try notCode <|> code) eof

chunks
  :: (Stream s (POR s) Char, Monoid s, CharSeq s)
  => ParserOpts s -> SourceName -> s -> Either ParseError [(Chunk s)]
chunks os n s = runReader (runParserT document () n s) os
