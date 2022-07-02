module GHCIStuffs where
import Regex
import RETree
import Text.Parsec
import Data.Either (fromRight)
prs :: String -> String
prs = fromRight "_" . fmap show . (parseRE :: String -> Either ParseError RETree)

type ParseResult = Either ParseError
