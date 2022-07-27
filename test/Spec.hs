import Test.Hspec
import Test.QuickCheck (quickCheckAll)

import QuickCheckRegex
import HspecRETree.RECompile

main :: IO ()
main = do
    quickCheckRegex
    hspec $ spec_parseRE
    return ()
