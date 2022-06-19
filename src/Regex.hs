module Regex
    ( Regex (..)
    , ParseResult
    ) where

import Text.Parsec (ParseError)

class Regex repr where
    parseRE :: String -> Either ParseError repr
    runRE :: repr -> String -> Bool

type ParseResult a = Either ParseError a