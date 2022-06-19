import QuickCheckRegex
import Test.QuickCheck (quickCheckAll)
main :: IO ()
main = do
    quickCheckRegex
    return ()
