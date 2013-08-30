module Test.SrcParserTest where

import Prelude hiding (succ, fail)
import Control.Applicative
import Text.Parsec hiding ((<|>))
import Test.QuickCheck
import Test.QuickCheck.All
import Narrative.SrcParser
import Narrative.CharSeq
import Data.String
import Data.Functor.Identity (Identity)
import Data.Char

instance (Eq s, Arbitrary s, CharSeq s) => Arbitrary (ParserOpts s) where
    arbitrary = suchThat pgen $ \(ParserOpts c i) -> c /= i
      where
        pgen = ParserOpts
            <$> (fromString <$> suchThat (listOf1 arbitrary) (not . isSpace . head))
            <*> (fromString <$> suchThat (listOf1 arbitrary) (not . isSpace . head))

instance (Arbitrary s, CharSeq s) => Arbitrary (ParserOpts s, [Chunk s]) where
    arbitrary = (,)
        <$> arbitrary
        <*> 

leftover' :: Monad m => ParsecT s u m a -> ParsecT s u m (Maybe a, s)
leftover' p = (,) <$> (try (Just <$> p) <|> pure Nothing) <*> getInput

testLeftover'
  :: Stream s Identity t
  => Parsec s () a
  -> (s -> (Maybe a, s) -> Bool)
  -> s
  -> Bool
testLeftover' p f s =
    either undefined (f s) $ parse (leftover' p) "" s

prop_nlOrEof :: String -> Property
prop_nlOrEof s =
    forAll (elements ["\n" ++ s, "\r\n" ++ s, s, ""])
    $ testLeftover' nlOrEof f
  where
    f ('\n':ss)      (Just _, l)  = ss == l
    f ('\r':'\n':ss) (Just _, l)  = ss == l
    f ""             (Just _, "") = True
    f _              (Just _, _)  = False

    f _ (Nothing, ('\n':_))    = False
    f _ (Nothing, '\r':'\n':_) = False
    f _ (Nothing, "")          = False
    f _ (Nothing, _)           = True

prop_manyCharTillSucc :: String -> Char -> String -> Property
prop_manyCharTillSucc s1 c s2 = not (any (== c) (s1 ++ s2)) ==>
    testLeftover' (manyCharTill anyChar (char c)) f $ s1 ++ c:s2
  where
    f _ (Just a, s2') = s2 == s2' && a == s1
    f _ _             = False

prop_manyCharTillFail :: String -> Char -> Property
prop_manyCharTillFail s c = not (any (== c) s) ==>
    testLeftover' (manyCharTill anyChar (char c)) f s
  where
    f _ (Just _, _)  = False
    f _ (Nothing, l) = s == l

prop_prefixedLine :: (String, String) -> Property
prop_prefixedLine (px, s) = (forAll $ listOf $ elements " \t") test
  where
    test ws = px /= "" && not (isSpace (head px)) ==>
              testLeftover' (prefixedLine px) (f ws) $ ws ++ px ++ s

    f ws _ (Just (ws', s1), l) = ws == ws' &&
                                 (s == s1
                                 || s == s1 ++ '\n':l
                                 || s == s1 ++ '\r':'\n':l)
    f ws _ (Nothing, l)        = l == ws ++ px ++ s

--prop_chunksRoundTrip :: [(Chunk String)] -> Bool
--prop_chunksRoundTrip

check :: IO Bool
check = $quickCheckAll

verboseCheck :: IO Bool
verboseCheck = $verboseCheckAll
