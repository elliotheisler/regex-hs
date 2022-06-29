{-# LANGUAGE OverloadedStrings #-}
module Util where

import Control.Monad
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Maybe (fromJust)


-- https://www.youtube.com/watch?v=PGsDvgmZF7A
readPairs :: FilePath -> IO [(Text, Text)]
readPairs csvFile =
  mapM toPairIO =<< Text.lines <$> Text.readFile csvFile

toPairIO :: Text -> IO (Text, Text)
toPairIO line = case Text.splitOn " / " line of
              [first, secnd] -> pure (first, secnd)
              -- must convert line to string to use String semigroup, i think
              _ -> fail ("invalid line (called toPairIO on failer.csv): " <> Text.unpack line) 

