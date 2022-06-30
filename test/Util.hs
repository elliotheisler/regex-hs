{-# LANGUAGE OverloadedStrings #-}
module Util
  ( readCSV
  ) where

import Control.Monad
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Maybe (fromJust)


readCSV :: Text -> FilePath -> IO [[Text]]
readCSV delim csvFile =
  map (\line -> Text.splitOn delim line) <$> Text.lines <$> Text.readFile csvFile

toPairIO :: Text -> Text -> IO (Text, Text)
toPairIO delim line = case Text.splitOn delim line of
              [first, secnd] -> pure (first, secnd)
              -- must convert line to string to use String semigroup, i think
              _ -> fail ("invalid line (called toPairIO on failer.csv): " <> Text.unpack line) 

