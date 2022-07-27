{-# LANGUAGE OverloadedStrings #-}
{-
with overloaded string literals enabled (with OverloadedStrings) a string literal has type (IsString a) => a.
This means that the usual string syntax can be used, e.g., for ByteString, Text, and other variations of string like types. String literals behave very much like integer literals 
-}

module HspecRETree.RECompile where


import Test.Hspec
import Text.Parsec (ParseError)

import Control.Monad
import Data.Semigroup -- for <> operator
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Text.Parsec

import Regex
import RETree
import TestUtil

type Description = String
type Input = String
type Result = Maybe RETree


spec_parseRE :: Spec
spec_parseRE = runMyTests test_parseRE "test/HspecRETree/RECompile.csv"
  where
    test_parseRE = rightToMaybe . reCompile
    rightToMaybe (Right r) = Just r
    rightToMaybe (Left _) = Nothing

runMyTests :: (Input -> Result) -> FilePath -> Spec
runMyTests fn csvFile = do
    tests <- map parseTestCase <$> runIO (readCommentedCSV csvDelim csvFile)
    forM_ tests $ \(description, input, expected) ->
      it description $
        fn input `shouldBe` expected

parseTestCase :: [Text] -> (Description, Input, Result)
parseTestCase [desc, input, expected] = -- let unpackedInput = Text.unpack input in
    ( Text.unpack $ substituteVars desc input expected
    , Text.unpack input
    , parseExpected expected
    )

parseTestCase _ = 
  error "MALFORMED TEST CASE: Tried to parseTriple on list not of length=3"


parseExpected :: Text -> Result
parseExpected "$FAIL" = Nothing
parseExpected regex = Just (read . Text.unpack $ regex :: RETree)


substituteVars :: Text -> Text -> Text -> Text 
substituteVars desc input expected =
  let words = Text.words desc
  in  if head words == "$THIS"
      then input <> " " <> (Text.unwords $ tail words)
      else desc

-- old test driver
-- spec_parseRE :: Spec
-- spec_parseRE = do
-- 
--     it "evaluates Repetition of zero times to empty regex" $
--       (reCompile "a{0,0}" :: Expected) `shouldBe` (Right $ read "Epsilon" :: Expected)
-- 
--     it "evaluates '|' (union of nothing) to empty regex" $
--       (reCompile "|" :: Expected) `shouldBe` (Right $ read "Epsilon" :: Expected)

