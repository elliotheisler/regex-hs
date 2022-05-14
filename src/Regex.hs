module Regex where

import Text.Parsec
import Text.Parsec.String
import Data.List (intersperse)
import Data.Maybe (catMaybes)

-- precedence decreases downward
-- TODO: add capture group constructor
-- TODO: add Null/Nothing constructor?
data RETree = 
    Symbol Char
  | Repetition RETree LowerBound UpperBound
  | Concat [RETree]
  | Union [RETree]
type LowerBound = Int
data UpperBound = Unlimited | Upper Int

instance Read RETree where
    read 

instance Show RETree where
    show reTree = show (Regex reTree) -- TODO: implement 'show' as a tree

instance Eq RETree where
    Symbol a == Symbol b = a == b
    Repetition ra la ua == Repetition rb lb ub = 
      ra == rb && la == lb && ua == ub
    Concat nodesA == Concat nodesB =
      all (\(a,b) -> a == b) $ zip nodesA nodesB
    Union nodesA == Concat nodesB =
      all (\(a,b) -> a == b) $ zip nodesA nodesB

instance Eq UpperBound where
    Unlimited == Unlimited = True
    Upper a == Upper b = a == b

newtype Regex = Regex { getRegex :: RETree }
instance Show Regex where
    show (Regex reTree) = "/" ++ unparse 0 reTree ++ "/"

brackets :: Int -> Int -> String -> String
brackets i j s
  | i >= j = "("++s++")"
  | otherwise = s

{- unparse: convert this regex tree to is string-regex form, making sure
 - to place it in brackets to show precedence if its parent has higher-or-equal 
 - precedence -}
unparse :: Int -> RETree -> String
unparse i (Symbol c) = [c]

unparse i (Repetition reNode lower Unlimited)
  | lower == 1 = brkts $ ps reNode ++ "+"
  | lower == 0 = brkts $ ps reNode ++ "*"
  | otherwise  = brkts $ ps reNode ++ "{" ++ show lower ++ ",}"
  where brkts = brackets i 3
        ps    = unparse 3
unparse i (Repetition reNode lower (Upper upper))
  | lower == upper = brkts $ ps reNode ++ "{" ++ show lower ++ "}"
  | otherwise = brkts $ ps reNode ++ "{" ++ show lower ++ "," ++ show upper ++ "}"
  where brkts = brackets i 3
        ps    = unparse 3

unparse i (Concat rs) = brackets i 2 . concat . map (unparse 2) $ rs

unparse i (Union rs) = brackets i 1 . concat . intersperse "|" . map (unparse 1) $ rs

-- '{' can optionally be treated literally in most dialects, 
--     so long as it doesnt denote a range ,for example, "a{1,2}"
-- these 12 are the same amongst most popular modern RE dialects
metaChars = "\\^$.|?*+()[{"
type REParser = Parsec String () RETree

-- TODO: parse quantifiers: *+?
-- TODO: capture groups with id:  ()
-- TODO: escaped characters & classes: \w, \t, \n
-- TODO: character classes: [a-zA-Z], etc
-- TODO: negated classes: [^a-z], etc
-- TODO: possesive & lazy quantifiers?

parseRegex :: REParser
parseRegex = do
    re <- parseUnion
    eof
    return re

parsePrimary :: REParser
parsePrimary = choice $ try <$> 
    [(between (char '(') (char ')') parseUnion),
     Symbol <$> (char '\\' >> oneOf metaChars), 
     Symbol <$> noneOf metaChars] 

parseRepetition :: REParser
parseRepetition = do
    prim <- parsePrimary
    range <- optionMaybe . try $ parseRange
    case range of Just (lower, upper) -> return (Repetition prim lower upper)
                  Nothing             -> return (Repetition prim 1 (Upper 1))

parseRange :: Parsec String () (LowerBound, UpperBound)
parseRange = do
    char '{'
    lower <- parseInt
    n <- try $ char ',' <|> char '}'
    if n == '}'
        then return (lower, Upper lower)
        else do
            upper <- optionMaybe . try $ parseInt
            char '}'
            case upper of Nothing -> return (lower, Unlimited)
                          Just u  -> 
                              if u >= lower 
                              then return (lower, Upper u  ) 
                              else fail "upper bound must be greater than lower"
    where parseInt = (read :: String -> LowerBound) <$> many1 digit

parseConcat :: REParser
parseConcat = Concat <$> many parseRepetition
                       
parseUnion :: REParser
parseUnion = Union <$> sepBy parseConcat (char '|')


trimFat :: RETree -> Maybe RETree
trimFat (Symbol s) = Just (Symbol s)

trimFat (Repetition reNode 1 (Upper 1)) = trimFat reNode
trimFat (Repetition reNode 0 (Upper 0)) = Nothing
trimFat (Repetition reNode lower upper) = 
    (\x -> Repetition x lower upper) <$> (trimFat reNode)

trimFat (Concat [reNode]) = trimFat reNode
trimFat (Concat reNodes ) = let trimmed = catMaybes (trimFat <$> reNodes)
                            in  case trimmed of [] -> Nothing
                                                reNodes -> Just (Concat reNodes)

trimFat (Union [reNode]) = trimFat reNode
trimFat (Union reNodes ) = let trimmed = catMaybes (trimFat <$> reNodes)
                           in  case trimmed of [] -> Nothing
                                               reNodes -> Just (Union reNodes)
