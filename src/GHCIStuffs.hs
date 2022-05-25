module GHCIStuffs where
import Regex
import Text.Parsec
import Data.Either (fromRight)
prs = fromRight (Symbol '_') . parse parseRegex ""
