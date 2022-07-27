{-# LANGUAGE OverloadedStrings #-}

module HspecRETree.REMatch -- TODO
    ( doSpec
    ) where

import Test.Hspec
import Control.Monad
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as Text

import Regex
import RETree
import Data.Maybe (isJust)
import TestUtil

type Description = String
type Input = (RETree, [String])
type Result = [Bool]
csvFile :: FilePath
csvFile = "test/HspecRETree/REMatch.csv"
unitUnderTest :: Input -> Result
unitUnderTest (reTree, inputs) = isJust <$> reMatch reTree <$> inputs
parseExpected :: Text -> Result
parseExpected = read . Text.unpack
parseInput :: Text -> Input
parseInput txt = (\(re,inputs) -> (fromRight undefined . reCompile $ re, inputs)) . read . Text.unpack $ txt



{-=-=-=-=-=-=-=-= dont change this stuff below =-=-=-=-=-=-=-=-}

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
