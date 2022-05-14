{-# LANGUAGE TemplateHaskell #-}

module TestRegex where

import Test.QuickCheck (Property, quickCheck, quickCheckAll, (==>))
import qualified Test.QuickCheck as QC
import Text.Parsec
import Regex
import Data.Either (isLeft)

{- helper function for tests. ensure the whole string must be parsed -}
parseAll :: REParser -> String -> Either ParseError RETree
parseAll parser = parse (entireString parser) ""

entireString :: REParser -> REParser
entireString parser = do
    s <- parser
    eof
    return s

{- parsing a string of one char yields (Symbol char) if-and-only-if that char is
 - not a meta-character -}
prop_Char0 c = (parseAll parsePrimary [c] == Right (Symbol c)) /= 
               (c `elem` metaChars)
prop_Char1 = not $ all (\ c -> (parseAll parsePrimary [c]) == Right (Symbol c)) metaChars

{- parsing "\c" yields (Symbol c) if-and-only-if c is a meta-character
 -}
prop_MetaChar0 = all (\ c -> (parse parsePrimary "" ['\\',c]) == Right (Symbol c)) metaChars
prop_MetaChar1 c = (parseAll parsePrimary ['\\', c] == Right (Symbol c)) == 
                   (c `elem` metaChars)

{- upper bound must be greater than lower bound -}
prop_RepetitionGreaterThan :: Char -> QC.NonNegative Int -> QC.NonNegative Int -> Property
prop_RepetitionGreaterThan c (QC.NonNegative l) (QC.NonNegative u) = 
  l <= u && not (c `elem` metaChars) ==> 
    (parseAll parseRepetition $ [c] ++ "{" ++ (show l) ++ "," ++ (show u) ++ "}") == 
      Right (Repetition (Symbol c) l (Upper u))
prop_RepetitionLessThan :: Char -> QC.NonNegative Int -> QC.NonNegative Int -> Property
prop_RepetitionLessThan c (QC.NonNegative l) (QC.NonNegative u) = 
    l > u ==> 
      (isLeft . parseAll parseRepetition $ [c] ++ "{" ++ (show l) ++ "," ++ (show u) ++ "}")

return []
runTests = $quickCheckAll