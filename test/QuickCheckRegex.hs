{-# LANGUAGE TemplateHaskell #-}

{- QuickCheckRegex: i wrote a little test suite in quickcheck to learn it. -}
{-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-}



module QuickCheckRegex where

import Test.QuickCheck (Property, quickCheck, quickCheckAll, (==>))
import qualified Test.QuickCheck as QC
import Text.Parsec
import Data.Either (isLeft)

import Regex
import RETree

{- parsing a string of one char yields (Symbol char) if-and-only-if that char is
 - not a meta-character -}
prop_Char0 c = (reCompile [c] == Right (Symbol c)) /= 
               (c `elem` mustBeEscaped)
prop_Char1 = not $ all (\ c -> (reCompile [c]) == Right (Symbol c)) mustBeEscaped

{- parsing "\c" yields (Symbol c) if-and-only-if c is a meta-character
 -}
prop_MetaChar0 = all (\ c -> (reCompile ['\\',c]) == Right (Symbol c)) mustBeEscaped
prop_MetaChar1 c = (reCompile ['\\', c] == Right (Symbol c)) == 
                   (c `elem` mustBeEscaped)

{- upper bound must be greater than lower bound -}
prop_QuantifierGreaterThan :: Char -> QC.NonNegative Int -> QC.NonNegative Int -> Property
prop_QuantifierGreaterThan c (QC.NonNegative l) (QC.NonNegative u) = 
  l <= u && not (c `elem` mustBeEscaped) && 1 < u ==> 
    ( ((reCompile :: String -> Either ParseError RETree) $ [c] ++ "{" ++ (show l) ++ "," ++ (show u) ++ "}") 
      == 
      Right (Q (Quantifier (Symbol c) l (Upper u) Greedy))
    )
prop_QuantifierLessThan :: Char -> QC.NonNegative Int -> QC.NonNegative Int -> Property
prop_QuantifierLessThan c (QC.NonNegative l) (QC.NonNegative u) = 
    l > u && not (c `elem` mustBeEscaped) ==> 
      (isLeft . (reCompile :: String -> Either ParseError RETree) $ [c] ++ "{" ++ (show l) ++ "," ++ (show u) ++ "}")

return []
quickCheckRegex = $quickCheckAll
