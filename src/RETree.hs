{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module RETree
    ( RETree (..)
    , LowerBound
    , UpperBound (..)
    , metaChars
    , showTreeAsRegex
    ) where

import Text.Parsec
import Text.Parsec.String
import Data.List (intersperse)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Either (isLeft)
import Text.Printf (printf)

import TreePrint
import Regex

-- precedence decreases downward
-- TODO: add capture group constructor
-- IN PROGRESS: add Null/Nothing constructor?
data RETree = 
    Epsilon -- the empty regular expression. matches everything
  | Symbol Char
  | Repetition RETree LowerBound UpperBound
  | Concat [RETree]
  | Union [RETree]
  deriving (Read)
type LowerBound = Int
data UpperBound = Unlimited | Upper Int deriving (Read)

instance Regex RETree where
    parseRE regex = (trimFat <$>) . parse parseRegex "" $ regex
    -- runRE Epsilon _ = True
    -- runRE (Symbol c) h:tail = c == h
    -- runRE (Repetition reTree lower Unlimited) str = 
    -- runRE (Union forest) str = 
                        

{-** PARSE-related functions & data **-}
{-****************************-}
type TreeParser = REParser RETree
-- '{' can optionally be treated literally in most dialects, 
--     so long as it doesnt denote a range ,for example, "a{1,2}"
-- this set of 12 are the same amongst most popular modern RE dialects
metaChars = "\\^$.|?*+()[{"
-- TODO: parse quantifiers: *+?
-- TODO: capture groups with id:  ()
-- TODO: escaped characters & classes: \w, \t, \n
-- TODO: character classes: [a-zA-Z], etc
-- TODO: negated classes: [^a-z], etc
-- TODO: possesive & lazy quantifiers?

parseRegex :: TreeParser
parseRegex = do
    re <- parseUnion
    eof
    return re
{- parsePrimary, parseRepetition, parseConcat, and parseUnion: 
   a typical recursive-descent parser, in bottom-up order. -}
parsePrimary :: TreeParser
parsePrimary = choice $ try <$> 
    [(between (char '(') (char ')') parseUnion)
    , Symbol <$> (char '\\' >> oneOf metaChars) 
    , Symbol <$> noneOf metaChars
    ]

parseRepetition :: TreeParser
parseRepetition = do
    prim  <- parsePrimary
    range <- optionMaybe parseRange -- parsing fails here if we have nonsensical range like '{7,3}'
    case range of Just (lower, upper) -> return (Repetition prim lower upper)
                  Nothing             -> return (Repetition prim 1 (Upper 1))

{- fails without consuming any input if range is malformed.
    fails *with* consuming input if upper bound is less than the lower bound
-}
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
                          Just u  -> if lower <= u
                                     then return $ (lower, (Upper u))
                                     else fail $ printf "malformed range: {%d,%d}" lower u
    where parseInt = (read :: String -> LowerBound) <$> many1 digit

parseConcat :: TreeParser
parseConcat = Concat <$> many parseRepetition
                       
parseUnion :: TreeParser
parseUnion = Union <$> sepBy parseConcat (char '|')

{- trimFat: remove redundant nodes, i.e. a union of one thing, repetition once or zero times, etc. -}
trimFat  :: RETree -> RETree
trimFat reTree = fromMaybe Epsilon $ trimFat' reTree
trimFat' :: RETree -> Maybe RETree
trimFat' (Symbol s) = Just (Symbol s)

trimFat' (Repetition reNode 1 (Upper 1)) = trimFat' reNode
trimFat' (Repetition reNode 0 (Upper 0)) = Nothing
trimFat' (Repetition reNode lower upper) = 
    (\x -> Repetition x lower upper) <$> (trimFat' reNode)

trimFat' (Concat [reTree]) = trimFat' reTree
trimFat' (Concat reForest  ) = let trimmed = catMaybes (trimFat' <$> reForest)
    in  case trimmed of [] -> Nothing
                        trimmedForest -> Just (Concat trimmedForest)

trimFat' (Union [reTree]) = trimFat' reTree
trimFat' (Union reForest  ) = let trimmed = catMaybes (trimFat' <$> reForest)
    in  case trimmed of [] -> Nothing
                        trimmedForest -> Just (Union trimmedForest)

instance Eq RETree where
    Epsilon == Epsilon = True
    Symbol a == Symbol b = a == b
    Repetition reTreeA lA uA == Repetition reTreeB lB uB = 
      reTreeA == reTreeB && lA == lB && uA == uB
    Concat forestA == Concat forestB =
      all (\(a,b) -> a == b) $ zip forestA forestB
    Union forestA == Concat forestB =
      all (\(a,b) -> a == b) $ zip forestA forestB
    _ == _ = False -- need to explicitly include default case, otherwise get
                   -- non-exhaustive patterns when running 'stack test'

instance Eq UpperBound where
    Unlimited == Unlimited = True
    Upper a == Upper b = a == b

{-** RUNREGEX-related functions **-}
{-****************************-}
-- TODO:

{-** SHOW-related functions **-}
{-****************************-}
instance Show RETree where
    show = treeShow -- from PrintableTree instance

instance PrintableTree RETree String where
    nodeStrContents = nodeContents
    
    nodeContents Epsilon = unParse 0 Epsilon
    nodeContents sym@(Symbol c) = unParse 0 sym
    nodeContents (Repetition _ l (Upper u))
      | l == 0 && u == 1 = "?"
      | l == u           = "{" ++ show l ++ "}"
      | otherwise        = "{" ++ show l ++ "," ++ show u ++ "}"
    nodeContents (Repetition _ l Unlimited) 
      | l == 0 = "(*)"
      | l == 1 = "(+)"
      | otherwise = "{" ++ show l ++ ",}"
    nodeContents (Concat _) = "(++)"
    nodeContents (Union  _) = "(|)"

    getForest Epsilon = []
    getForest (Symbol _) = []
    getForest (Repetition reTree _ _) = [reTree]
    getForest (Concat reTrees) = reTrees
    getForest (Union  reTrees) = reTrees

{- showTreeAsRegex :: exactly what you would expect :)
-}
showTreeAsRegex :: RETree -> String
showTreeAsRegex reTree = "/" ++ unParse 0 reTree ++ "/"
{- unParse: convert this regex tree to is string-regex form, making sure
   to place it in brackets to show precedence if its parent has higher-or-equal 
   precedence 
-}

unParse :: Int -> RETree -> String
unParse _ Epsilon = "\x03f5"

unParse _ (Symbol c) = [c]

unParse i (Repetition reNode lower Unlimited)
  | lower == 1 = brkts $ ps reNode ++ "+"
  | lower == 0 = brkts $ ps reNode ++ "*"
  | otherwise  = brkts $ ps reNode ++ "{" ++ show lower ++ ",}"
  where brkts = brackets i 3
        ps    = unParse 3
unParse i (Repetition reNode lower (Upper upper))
  | lower == upper = brkts $ ps reNode ++ "{" ++ show lower ++ "}"
  | otherwise = brkts $ ps reNode ++ "{" ++ show lower ++ "," ++ show upper ++ "}"
  where brkts = brackets i 3
        ps    = unParse 3

unParse i (Concat rs) = brackets i 2 . concat . map (unParse 2) $ rs

unParse i (Union rs) = brackets i 1 . concat . intersperse "|" . map (unParse 1) $ rs

{- brackets: helper function for unParse. 
   if precedence 'j' of current node is *not* lower than that of the parent node 'i', wrap
   this node's representation in brackets to explicitly show that this node is a child.
-}
brackets :: Int -> Int -> String -> String
brackets i j s
  | i >= j = "("++s++")"
  | otherwise = s
