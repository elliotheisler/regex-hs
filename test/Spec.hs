import Test.Hspec
import Test.QuickCheck (quickCheckAll)

import QuickCheckRegex
import HspecRETree.RECompile
import qualified HspecRETree.RECompile as RECompile
import qualified HspecRETree.REMatch as REMatch

main :: IO ()
main = do
    quickCheckRegex
    hspec $ RECompile.doSpec
    hspec $ REMatch.doSpec
    return ()
