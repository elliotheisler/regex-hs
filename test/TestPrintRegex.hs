module TestPrintRegex where
    
import Text.Parsec
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)

import Regex

testPrintRegex :: IO ()
testPrintRegex = do
    let email = fromRight (Symbol '_') . parse parseRegex "" $ "(A|B|C|D|E|a|b|c|d|e|_){1,}@(gmail|hotmail|outlook|aol)\\.(com|org|net)"
    print email
    let emailTrimmed = fromMaybe (Symbol '_') $ trimFat email
    print emailTrimmed