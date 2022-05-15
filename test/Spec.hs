import TestRegex
import TestPrintRegex (testPrintRegex)
import Test.QuickCheck (quickCheckAll)
main :: IO ()
main = do
    runTests
    testPrintRegex
    return ()
