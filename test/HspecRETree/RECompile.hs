{-# LANGUAGE OverloadedStrings #-}

module HspecRETree.RECompile
    ( doSpec
    ) where

import Test.Hspec
import Control.Monad
import TestUtil

import Text.Parsec (ParseError)
import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Control.Monad
import Data.Semigroup -- for <> operator?

import Regex
import RETree

type Description = String
type Input = String
type Result = Maybe RETree
csvFile :: FilePath
csvFile = "test/HspecRETree/RECompile.csv"
unitUnderTest :: Input -> Result
unitUnderTest = rightToMaybe . reCompile
  where
    rightToMaybe (Right r) = Just r
    rightToMaybe (Left _) = Nothing

parseExpected :: Text -> Result
parseExpected "$FAIL" = Nothing
parseExpected regex = Just (read . Text.unpack $ regex :: RETree)

parseInput :: Text -> Input
parseInput = Text.unpack

doSpec :: Spec
doSpec = runMyTests unitUnderTest csvFile

runMyTests :: (Input -> Result) -> FilePath -> Spec
runMyTests fn csvFile = do
    tests <- map parseTestCase <$> runIO (readCommentedCSV csvDelim csvFile)
    forM_ tests $ \(description, input, expected) ->
      it description $
        fn input `shouldBe` expected

parseTestCase :: [Text] -> (Description, Input, Result)
parseTestCase [desc, input, expected] =
    ( Text.unpack $ substituteVars desc input expected
    , parseInput input
    , parseExpected expected
    )

parseTestCase _ = 
  error "MALFORMED TEST CASE: Tried to parseTriple on list not of length=3"
