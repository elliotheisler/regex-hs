module TestRegex where
import Text.Parsec
import Test.QuickCheck (quickCheck)
import Regex

{- parsing a string of one char yields (Symbol char) if-and-only-if that char is
 - not a meta-character -}
prop_Char c = (parse parsePrimary "" [c] == Right $ Symbol c) /= 
                 (c `elem` metaChars)

{- parsing "\c" yields (Symbol c) if-and-only-if c is a meta-character
 -}
prop_MetaChar c = (parse parsePrimary "" ['\\', c] == Right $ Symbol c) == 
                     (c `elem` metaChars)


