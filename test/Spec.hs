import Test.Hspec
import Test.QuickCheck (quickCheckAll)

import QuickCheckRegex
import HspecRETree.RECompile
import qualified HspecRETree.RECompile as RECompile
import qualified HspecRETree.REMatch as REMatch

main :: IO ()
main = do
    putStrLn "TESTING REGEX PARSING (using QuickCheck)\n========================================"
    quickCheckRegex
    putStrLn "TESTING REGEX PARSING (using Hspec)\n========================================"
    hspec RECompile.doSpec
    putStrLn "TESTING REGEX EXECUTION (using HSpec)\n========================================"
    hspec REMatch.doSpec
