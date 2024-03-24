module GHCIStuffs
  ( mkRegex,
    module E,
  )
where

import Data.Either as E
import RETree
import Regex
import Text.Parsec

-- reCompile wrapper for the CLI
mkRegex :: String -> RETree
mkRegex str = E.fromRight (error "invalid regular expression") (reCompile str)

type ParseResult = Either ParseError
