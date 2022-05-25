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
    let basic = getTree "a{0,}b{1,}c{5}d{5,}e"
    p . show . Regex $ basic
    p . show $ basic
    let basic = getTree "(abcde)f"
    p . show . Regex $ basic
    p . show $ basic
    let basic = getTree "(abcde){0,}"
    p . show . Regex $ basic
    p . show $ basic
  where
    p str = putStrLn $ str++"\n"
    getStr :: String -> String
    getStr = show . getTree
    getTree = fromRight (Symbol '_') . parse parseRegex ""
