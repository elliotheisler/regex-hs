import Test.Hspec
import Test.QuickCheck (quickCheckAll)

import QuickCheckRegex
import HspecRETree

main :: IO ()
main = do
    quickCheckRegex
    hspec $ spec_parseRE
    return ()
