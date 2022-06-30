{-# LANGUAGE OverloadedStrings #-}
module TestUtil
  ( readCSV
  , readCommentedCSV
  , csvDelim
  ) where

import Control.Monad
import Data.Semigroup
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Maybe (fromJust)

csvDelim = " / " :: Text

readCSV :: Text -> FilePath -> IO [[Text]]
readCSV delim csvFile = sepValues delim <$> pruneEmptys <$> Text.lines <$> Text.readFile csvFile

readCommentedCSV :: Text -> FilePath -> IO [[Text]]
readCommentedCSV delim csvFile =
    sepValues delim <$> pruneComments <$> pruneEmptys <$> Text.lines <$> Text.readFile csvFile
  where
    pruneComments :: [Text] -> [Text]
    pruneComments = filter $ not . ("#" `isPrefixOf`)

sepValues :: Text -> ( [Text] -> [[Text]] )
sepValues delim = map (\line -> Text.splitOn delim line)

pruneEmptys :: [Text] -> [Text]
pruneEmptys = filter (not . Text.null)
