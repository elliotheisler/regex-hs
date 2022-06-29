import Test.Hspec
import Test.QuickCheck (quickCheckAll)

import QuickCheckRegex
import HspecRETree.ParseRE

main :: IO ()
main = do
    quickCheckRegex
    hspec $ spec_parseRE
    return ()
