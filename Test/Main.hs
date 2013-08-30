module Main where

import qualified Test.SrcParserTest

main :: IO ()
main = do
    Test.SrcParserTest.check
    return ()
