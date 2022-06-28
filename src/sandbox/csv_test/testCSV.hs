{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Maybe (fromJust)


{- ================================================= -}
readPairsTravrs :: FilePath -> IO ( Maybe [(Text, Text)] )
readPairsTravrs csvFile =
  toPairsTravrs <$> Text.lines <$> Text.readFile csvFile

toPairsTravrs :: [Text] -> Maybe [(Text, Text)]
toPairsTravrs = traverse
    ( \ line ->
        case Text.splitOn "," line of
            [first, secnd] -> Just (first , secnd)
            _ -> Nothing
    )


{- ================================================= -}
readPairsFld :: FilePath -> IO ( Maybe [(Text, Text)] )
readPairsFld csvFile =
  toPairsFld <$> Text.lines <$> Text.readFile csvFile

toPairsFld :: [Text] -> Maybe [(Text, Text)]
toPairsFld lines = foldl pairFolder (Just []) lines

pairFolder :: Maybe [(Text, Text)] -> Text -> Maybe [(Text, Text)]
pairFolder Nothing _ = Nothing
pairFolder (Just acc) line = case Text.splitOn "," line of
    [first, secnd] -> Just $ acc ++ [(first, secnd)]
    _ -> Nothing


{- ================================================= -}
readPairsSimple :: FilePath -> IO ( Maybe [(Text, Text)] )
readPairsSimple csvFile =
  toPairsSimple <$> Text.lines <$> Text.readFile csvFile

toPairsSimple :: [Text] -> Maybe [(Text, Text)]
toPairsSimple [] = Just []
toPairsSimple (line:lines) = case Text.splitOn "," line of
    [first, secnd] -> (first, secnd) `maybeCons` (toPairsSimple lines)
    _ -> Nothing
  where
    maybeCons _ Nothing = Nothing
    maybeCons a (Just rest) = Just $ a : rest


{- ================================================= -}
-- https://www.youtube.com/watch?v=PGsDvgmZF7A
readPairsMapIO :: FilePath -> IO [(Text, Text)]
readPairsMapIO csvFile =
  mapM toPairIO =<< Text.lines <$> Text.readFile csvFile

toPairIO :: Text -> IO (Text, Text)
toPairIO line = case Text.splitOn "," line of
              [first, secnd] -> pure (first, secnd)
              -- must convert line to string to use String semigroup, i think
              _ -> fail ("invalid line (called toPairIO on failer.csv): " <> Text.unpack line) 


{- ======================main======================= -}
main :: IO ()
main = do
    putStrLn $ "\nrunning Simple..."
    success <- readPairsSimple "success.csv"
    failure <- readPairsSimple "failure.csv"
    putStrLn $ show success
    putStrLn $ show failure

    putStrLn $ "\nrunning Fold..."
    success <- readPairsFld "success.csv"
    failure <- readPairsFld "failure.csv"
    putStrLn $ show success
    putStrLn $ show failure

    putStrLn $ "\nrunning Traverse..."
    success <- readPairsTravrs "success.csv"
    failure <- readPairsTravrs "failure.csv"
    putStrLn $ show success
    putStrLn $ show failure
    putStrLn $ "\nrunning Default..."
    success <- readPairsMapIO "success.csv"
    failure <- readPairsMapIO "failure.csv"
    putStrLn $ show success
    putStrLn $ show failure
