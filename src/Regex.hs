module Regex
    ( Regex (..)
    , RegularExpr
    , REParser
    , metaChars
    , mustBeEscaped
    ) where

import Text.Parsec

type RegularExpr = String

class Regex repr where
    parseRE :: RegularExpr -> Either ParseError repr
    runRE :: repr -> String -> Bool

type REParser a = Parsec String () a

-- '{' can optionally be treated literally in most dialects, 
--     so long as it doesnt denote a range ,for example, "a{1,2}"
-- this set of 12 are the same amongst most popular modern RE dialects
metaChars = "\\^$.|?*+()[{"
mustBeEscaped = "/\\^$.|?*+()["
-- TODO: parse quantifiers: *+?
-- TODO: capture groups with id:  ()
-- TODO: escaped characters & classes: \w, \t, \n
-- TODO: character classes: [a-zA-Z], etc
-- TODO: negated classes: [^a-z], etc
-- TODO: possesive & lazy quantifiers?

-- this just hides underlying type too much: type ParseResult a = Either ParseError a

-- doesn't work because not all data constructors are public for ParseError
-- {-# LANGUAGE StandaloneDeriving #-}
-- deriving instance Read ParseError
