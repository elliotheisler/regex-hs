{-# LANGUAGE TemplateHaskell #-}

module TestRegex where

import Text.Parsec
import Test.QuickCheck (quickCheck, quickCheckAll)
import Regex

{- parsing a string of one char yields (Symbol char) if-and-only-if that char is
 - not a meta-character -}
prop_Char0 c = (parse parsePrimary "" [c] == Right (Symbol c)) /= 
                 (c `elem` metaChars)
prop_Char1 = not $ all (\ c -> (parse parsePrimary "" [c]) == Right (Symbol c)) metaChars

{- parsing "\c" yields (Symbol c) if-and-only-if c is a meta-character
 -}
prop_MetaChar0 = all (\ c -> (parse parsePrimary "" ['\\',c]) == Right (Symbol c)) metaChars
prop_MetaChar1 c = (parse parsePrimary "" ['\\', c] == Right (Symbol c)) == 
                     (c `elem` metaChars)

prop_Rep

return []
runTests = $quickCheckAll