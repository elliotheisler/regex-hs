{-# LANGUAGE OverloadedStrings #-}
module TestUtil
  ( readCSV
  , readCommentedCSV
  , csvDelim
  , substituteVars
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

substituteVars :: Text -> Text -> Text -> Text 
substituteVars desc input expected =
    Text.unwords . swapOnPred (=="$THIS") input . Text.words $ desc
  where
    swapOnPred :: (a -> Bool) -> a -> [a] -> [a]
    swapOnPred p a list = case remainder of
        _:tail -> acc <> (a : tail)
        [] -> acc
      where
        (acc, remainder) = splitWhen p list
    splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
    splitWhen p l = 
        (reverse acc, remainder)
      where
        (acc, remainder) = splitWhen' p [] l
    splitWhen' :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
    splitWhen' p acc [] = (acc, [])
    splitWhen' p acc l@(h:tail)
      | p h = (acc, l)
      | otherwise = splitWhen' p (h:acc) tail