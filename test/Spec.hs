import Test.Hspec
import Test.QuickCheck (quickCheckAll)

import QuickCheckRegex
import HspecRETree.RECompile
import qualified HspecRETree.RECompile as RECompile

main :: IO ()
main = do
    quickCheckRegex
    hspec $ RECompile.doSpec
    return ()
