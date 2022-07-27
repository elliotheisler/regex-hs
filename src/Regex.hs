module Regex
    ( RegexRepr (..)
    , MatchProgress (..)
    , REParser
    , metaChars
    , escapedStandinChars
    , classChars
    , lookupStandin
    , lookupClass
    ) where

import Text.Parsec

class RegexRepr r where
    -- parse a string to internal representation
    reCompile :: String -> Either ParseError r

    reMatches :: r -> String -> [MatchProgress] -- all matches from start of input

    reMatchesSearch :: r -> String -> [MatchProgress] -- all matches starting from every position of the input
    reMatchesSearch r input = foldl (\ acc str -> acc <> reMatches r str) [] . suffixes $ input
      where
        suffixes :: [a] -> [[a]]
        suffixes [] = [[]]
        suffixes l@(h:tail) = l : suffixes tail

    reMatch :: r -> String -> Maybe MatchProgress -- first match from start of input
    reMatch r input = case reMatches r input of
        h:tail -> Just h
        [] -> Nothing

    -- try to match from first char, second, etc. and stop at first match
    reMatchSearch :: r -> String -> Maybe MatchProgress 
    reMatchSearch r "" = reMatch r ""
    reMatchSearch r input@(c:tail) = case reMatch r input of
        Just m -> Just m
        Nothing -> reMatch r tail

-- string containing input parsed so far
-- string containing remainder of input
-- list of progress states snapshotted by inner capture groups so far
data MatchProgress = MatchProgress String String [MatchProgress] deriving (Eq)

instance Show MatchProgress where
    show (MatchProgress consumed remaining groups) = 
        "\nConsumed : " <> (show . reverse $ consumed) <> 
        "\nRemaining: " <> show remaining <> 
        "\nGroups   : " <> show groups <> "\n"

type REParser a = Parsec String () a

-- '{' can optionally be treated literally in most dialects, 
--     so long as it doesnt denote a range, for example, "a{1,2}"
-- this set of 12 are the same amongst most popular modern RE dialects
quantifierChars = "?*+"
bracketChars = "()["
assertionTokens = ["\b", "^", "$"] -- chars that assert something about current state without consuming input
metaChars = "/\\^$.|" <> bracketChars <> quantifierChars -- characters that always have special meaning. they must be escaped to be treated literally
classChars = "w" -- TODO: chars that stand in for a character class when escaped
escapedStandinChars = "a" -- chars that stand in for a different character when escaped

-- TODO:
lookupStandin :: Char -> Char
lookupStandin c = case c of
    'a' -> '\x0007'
    _   -> undefined

-- TODO: lookupClass :: Char -> CharClass
-- because need to handle negated classes like \W
lookupClass :: Char -> [Char]
lookupClass c = case c of
    'w' -> ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_"
    _  -> undefined
-- TODO: escaped characters classes: \w, \t, \n
-- TODO: character classes: [a-zA-Z], etc
-- TODO: negated classes: [^a-z], etc
-- TODO: possesive & lazy quantifiers?

-- doesn't work because data constructors are not public for ParseError
-- {-# LANGUAGE StandaloneDeriving #-}
-- deriving instance Read ParseError
