module Test.SrcParserTest where

import Prelude hiding (succ, fail)
import Control.Applicative
import Text.Parsec
import Test.QuickCheck
import Test.QuickCheck.All
import Narrative.SrcParser
import Data.Functor.Identity (Identity)

leftover :: Monad m => ParsecT s u m a -> ParsecT s u m (a, s)
leftover p = (,) <$> p <*> getInput

testLeftover
  :: Stream s Identity t
  => Parsec s () a
  -> (s -> (a, s) -> Bool)
  -> (s -> ParseError -> Bool)
  -> s
  -> Bool
testLeftover p succ fail s =
    either (fail s) (succ s) $ parse (leftover p) "" s

prop_nlOrEof :: String -> Property
prop_nlOrEof s =
    forAll (elements ["\n" ++ s, s, ""])
    $ testLeftover nlOrEof succ fail
  where
    succ ('\n':ss) (_, ss') = ss == ss'
    succ ""        (_, "")  = True
    succ _         _        = False

    fail ('\n':_) _ = False
    fail ""       _ = False
    fail _        _ = True

check :: IO Bool
check = $quickCheckAll

verboseCheck :: IO Bool
verboseCheck = $verboseCheckAll
