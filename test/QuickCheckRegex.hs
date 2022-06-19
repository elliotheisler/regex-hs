{-# LANGUAGE TemplateHaskell #-}

module QuickCheckRegex where

import Test.QuickCheck (Property, quickCheck, quickCheckAll, (==>))
import qualified Test.QuickCheck as QC
import Text.Parsec
import Data.Either (isLeft)

import Regex
import RETree

{- parsing a string of one char yields (Symbol char) if-and-only-if that char is
 - not a meta-character -}
prop_Char0 c = (parseRE [c] == Right (Symbol c)) /= 
               (c `elem` metaChars)
prop_Char1 = not $ all (\ c -> (parseRE [c]) == Right (Symbol c)) metaChars

{- parsing "\c" yields (Symbol c) if-and-only-if c is a meta-character
 -}
prop_MetaChar0 = all (\ c -> (parseRE ['\\',c]) == Right (Symbol c)) metaChars
prop_MetaChar1 c = (parseRE ['\\', c] == Right (Symbol c)) == 
                   (c `elem` metaChars)

{- upper bound must be greater than lower bound -}
prop_RepetitionGreaterThan :: Char -> QC.NonNegative Int -> QC.NonNegative Int -> Property
prop_RepetitionGreaterThan c (QC.NonNegative l) (QC.NonNegative u) = 
  l <= u && not (c `elem` metaChars) && 1 < u ==> 
    ( ((parseRE :: String -> Either ParseError RETree) $ [c] ++ "{" ++ (show l) ++ "," ++ (show u) ++ "}") 
      == 
      Right (Repetition (Symbol c) l (Upper u))
    )
prop_RepetitionLessThan :: Char -> QC.NonNegative Int -> QC.NonNegative Int -> Property
prop_RepetitionLessThan c (QC.NonNegative l) (QC.NonNegative u) = 
    l > u && not (c `elem` metaChars) ==> 
      (isLeft . (parseRE :: String -> Either ParseError RETree) $ [c] ++ "{" ++ (show l) ++ "," ++ (show u) ++ "}")

return []
quickCheckRegex = $quickCheckAll