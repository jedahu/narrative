module Main where

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.SrcParserTest

main :: IO ()
main = Test.SrcParserTest.check >> return ()
