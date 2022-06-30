{-# LANGUAGE OverloadedStrings #-}
{-
with overloaded string literals enabled (with OverloadedStrings) a string literal has type (IsString a) => a.

This means that the usual string syntax can be used, e.g., for ByteString, Text, and other variations of string like types. String literals behave very much like integer literals 
-}

module HspecRETree.ParseRE where


import Test.Hspec
import Text.Parsec (ParseError)

import Control.Monad
import Data.Semigroup -- for <> operator
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Regex
import RETree
import Util

type Description = String
type Input = String
type Expected = Either ParseError RETree

spec_parseRE :: Spec
spec_parseRE = do
    tests <- map parseTest <$> runIO (readCSV " / " "test/HspecRETree/ParseRE.csv")
    forM_ tests $ \(description, input, expected) ->
      it description $
        parseRE input `shouldBe` expected

parseTest :: [Text] -> (Description, Input, Expected)
parseTest [desc, input, expected] = -- let unpackedInput = Text.unpack input in
    ( Text.unpack $ substituteVars desc input expected
    , Text.unpack input
    , Right (read . Text.unpack $ expected :: RETree)
    )
parseTest _ = error "Tried to parseTriple on list not of length=3"

substituteVars :: Text -> Text -> Text -> Text 
substituteVars desc input expected =
  let words = Text.words desc
  in  if head words == "$THIS"
      then input <> " " <> (Text.unwords $ tail words)
      else desc

-- spec_parseRE :: Spec
-- spec_parseRE = do
-- 
--     it "evaluates Repetition of zero times to empty regex" $
--       (parseRE "a{0,0}" :: Expected) `shouldBe` (Right $ read "Epsilon" :: Expected)
-- 
--     it "evaluates '|' (union of nothing) to empty regex" $
--       (parseRE "|" :: Expected) `shouldBe` (Right $ read "Epsilon" :: Expected)

