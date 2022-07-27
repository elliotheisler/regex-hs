module GHCIStuffs 
    ( prs
    , module E
    ) where

import Regex
import RETree
import Text.Parsec
import Data.Either as E
prs :: String -> String
prs = fromRight "_" . fmap show . (reCompile :: String -> Either ParseError RETree)

type ParseResult = Either ParseError
