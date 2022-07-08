{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module RETree
    ( RETree (..)
    , LowerBound
    , UpperBound (..)
    , metaChars
    , showTreeAsRegex
    , reAllMatches
--    , reSearchRep
    , reSearch
    , ParseProgress (..)
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
data RETree = 
    Epsilon -- the empty regular expression. matches everything
  | Symbol Char
  | Repetition RETree LowerBound UpperBound
  | Concat [RETree]
  | Union [RETree]
  deriving (Read)
type LowerBound = Int
data UpperBound = Unlimited | Upper Int deriving (Read, Show, Eq)

instance Regex RETree where
    parseRE regex = (trimFat <$>) . parse parseRegex "" $ regex
    -- runRE Epsilon _ = True
    -- runRE (Symbol c) h:tail = c == h
    -- runRE (Repetition reTree lower Unlimited) str = 
    -- runRE (Union forest) str = 
                        


{-** PARSE-related functions & data **-}
{-************************************************************************************-}
type TreeParser = REParser RETree

parseRegex :: TreeParser
parseRegex = parseUnion <* eof

{- parsePrimary, parseRepetition, parseConcat, and parseUnion: 
   a typical recursive-descent parser, presented in bottom-up order. -}
parsePrimary :: TreeParser
parsePrimary = choice $ try <$> 
    [(between (char '(') (char ')') parseUnion)
    , Symbol <$> (char '\\' >> oneOf mustBeEscaped) 
    , Symbol <$> noneOf mustBeEscaped
    ]

parseRepetition :: TreeParser
parseRepetition = do
    prim  <- parsePrimary
    range <- optionMaybe . try $ parseRange 
    case range of Just (lower, Upper upper) 
                    | lower <= upper -> return (Repetition prim lower (Upper upper))
                    | otherwise -> fail "parsed an invalid range (upper bound less than lower bound)"
                  Just (lower, Unlimited) -> return (Repetition prim lower Unlimited)
                  Nothing                 -> return (Repetition prim 1 (Upper 1))

{- fails without consuming any input if range is malformed.
    fails *with* consuming input if a range was parsed, 
    but upper bound was less than the lower bound
-}
parseRange :: Parsec String () (LowerBound, UpperBound)
parseRange = do
    char '{'
    --TODO: parsing will not get past here on "a{b": unexpected b, expecting digit
    lower <- try parseInt 
    n <- try $ char ',' <|> char '}'
    if n == '}'
        then return (lower, Upper lower)
        else do
            upper <- optionMaybe . try $ parseInt
            char '}'
            case upper of Nothing -> return (lower, Unlimited)
                          Just u  -> return (lower, Upper u  )
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
                        [reTree] -> Just reTree
                        trimmedForest -> Just (Union trimmedForest)

-- TODO: is this just deriving (Eq)?
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



{-** RUNREGEX-related functions **-}
{-************************************************************************************-}
-- TODO:
-- (RETree, Parsed, Remaining) => [(Parsed, Remaining)]

data ParseProgress = ParseProgress String String deriving (Eq, Show)

reAllMatches :: RETree -> String -> [String]
reAllMatches reTree input = ( \(ParseProgress prsed _) -> reverse prsed ) <$> srchResults
  where
    srchResults = reSearch reTree (ParseProgress "" input)

reSearch :: RETree -> ParseProgress -> [ParseProgress]

reSearch Epsilon state = pure state

reSearch (Union []) _ = mempty
reSearch (Union _) (ParseProgress _ "") = mempty
reSearch (Union (reNode:reForest)) state = 
    reSearch reNode state <> reSearch (Union reForest) state
    
reSearch (Concat []) state = pure state -- simply return state wrapped in functor
reSearch (Concat _) (ParseProgress _ "") = mempty
reSearch (Concat (reNode:reForest)) state =
    reSearch reNode state >>= reSearch (Concat reForest) 

reSearch (Repetition reTree lower upper) state@(ParseProgress consumed remaining) =
    reSearchRep [] subSearcher (lower, upper) 0 state
  where
    subSearcher = reSearch reTree
  
reSearch (Symbol _) (ParseProgress _ "") = mempty
reSearch (Symbol s) (ParseProgress consumed (r:remaining))
  | s == r = [ParseProgress (r:consumed) remaining]
  | otherwise = mempty

type Range = (LowerBound, UpperBound)

reSearchRep 
    :: [ParseProgress]                    
    -> (ParseProgress -> [ParseProgress]) 
    -> Range                              
    -> Int                                
    -> (ParseProgress -> [ParseProgress])

reSearchRep acc _ (lower, Upper upper) i state@(ParseProgress _ "") = acc
reSearchRep acc f range@(lower, Upper upper) i state
    | nextStates == mempty = acc
    | i >  upper = undefined
    | i == upper = acc -- reached the max number of repetitions, now return the collection of states
    | i >= lower - 1 = nextStates >>= reSearchRep (nextStates <> acc) f range (i+1) -- accumulate next states
    | otherwise      = nextStates >>= reSearchRep acc f range (i+1) -- parse another repetition but don't accumulate
  where
    nextStates = f state

{- | ~=~: "extensional equivalence"
   a ~=~ b is true iff when applied to a higher order function (i.e. reSearch), a and b 
   produce extensionally equivalent functions. this should be true if-and-only-if their 
   minimized (trimFat) versions are structurally equal
-}
infix 4 ~=~
(~=~) :: RETree -> RETree -> Bool
treeA ~=~ treeB = trimmedA == trimmedB
  where
    trimmedA = trimFat treeA
    trimmedB = trimFat treeB



{-** SHOW-related functions **-}
{-************************************************************************************-}
instance Show RETree where
    show = ptShow -- from PrintableTree instance

instance PrintableTree RETree where
    ptContents Epsilon = unParse 0 Epsilon
    ptContents sym@(Symbol c) = unParse 0 sym
    ptContents (Repetition _ l (Upper u))
      | l == 0 && u == 1 = "?"
      | l == u           = "{" ++ show l ++ "}"
      | otherwise        = "{" ++ show l ++ "," ++ show u ++ "}"
    ptContents (Repetition _ l Unlimited) 
      | l == 0 = "(*)"
      | l == 1 = "(+)"
      | otherwise = "{" ++ show l ++ ",}"
    ptContents (Concat _) = "(++)"
    ptContents (Union  _) = "(|)"

    ptForest Epsilon = []
    ptForest (Symbol _) = []
    ptForest (Repetition reTree _ _) = [reTree]
    ptForest (Concat reTrees) = reTrees
    ptForest (Union  reTrees) = reTrees

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
