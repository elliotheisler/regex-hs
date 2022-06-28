module Regex
    ( Regex (..)
    , RegularExpr
    , REParser
    ) where

import Text.Parsec

type RegularExpr = String

class Regex repr where
    parseRE :: RegularExpr -> Either ParseError repr
    runRE :: repr -> String -> Bool

type REParser a = Parsec String () a
-- this just hides underlying type too much: type ParseResult a = Either ParseError a

-- doesn't work because not all data constructors are public for ParseError
-- {-# LANGUAGE StandaloneDeriving #-}
-- deriving instance Read ParseError
