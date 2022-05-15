module TestPrintRegex where
    
import Text.Parsec
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)

import Regex

testPrintRegex :: IO ()
testPrintRegex = do
    let basic = getStr "a"
    p basic
    let basic = getStr "a|b"
    p basic
    let basic = getStr "ab|cd"
    p basic
    let basic = getTree "(ab){0,}"
    p . show . Regex $ basic
    p . show $ basic
  where
    p str = putStrLn $ str++"\n"
    getStr :: String -> String
    getStr = show . getTree
    getTree = fromRight (Symbol '_') . parse parseRegex ""