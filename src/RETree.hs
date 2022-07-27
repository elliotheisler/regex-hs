{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module RETree
    ( RETree (..)
    , Quantifier (..)
    , LowerBound
    , UpperBound (..)
    , LazyOrGreedy (..)
    , metaChars
    , unparseTree
--    , reSearchQfied
    , MatchProgress (..)
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
  | CaptureGroup RETree
  | Symbol Char
  | Q Quantifier 
  | Concat [RETree]
  | Union [RETree]
  deriving (Eq, Read)

-- Quantified regex's have their own special complexity so are defined as a seperate type
data Quantifier = Quantifier RETree LowerBound UpperBound LazyOrGreedy deriving (Eq, Read)
type LowerBound = Int
data UpperBound = Unlimited | Upper Int deriving (Read, Show, Eq)

instance Ord UpperBound where
    _ <= Unlimited = True
    Upper r <= Upper l = r <= l

data LazyOrGreedy = Lazy | Greedy deriving (Eq, Read)

instance RegexRepr RETree where
    reCompile regex = (trimFat <$>) . parse (parseUnion <* eof) "" $ regex
    reMatches reTree input = reMatches_ reTree (MatchProgress "" input [])

    -- reMatchSearch Epsilon input = Just (MatchProgress [] input [])
    -- reMatchSearch r "" = Nothing
    -- reMatchSearch r input@(c:tail) = case reMatch r input of
    --     Just m -> Just m
    --     Nothing -> reMatch r tail
                        


{-** PARSE-related functions & data **-}
{-************************************************************************************-}
type TreeParser = REParser RETree
{- parsePrimary, parseQuantifier, parseConcat, and parseUnion: 
   a typical recursive-descent parser, presented in bottom-up order. -}
                       
parseUnion :: TreeParser
parseUnion = Union <$> sepBy parseConcat (char '|')

parseConcat :: TreeParser
parseConcat = Concat <$> many parseQuantifier

parsePrimary :: TreeParser
parsePrimary = choice $ try <$> 
    [ CaptureGroup <$> (between (char '(') (char ')') parseUnion)
    , Symbol <$> (char '\\' >> oneOf mustBeEscaped) 
    , Symbol <$> noneOf mustBeEscaped
    ]

parseQuantifier :: TreeParser
parseQuantifier = do
    prim  <- parsePrimary
    range <- optionMaybe parseQuantifier'
    case range of Just (lower, upper, lazyOrGreed) 
                    | (Upper lower) <= upper -> return (Q (Quantifier prim lower upper lazyOrGreed))
                    | otherwise -> fail "parsed an invalid range (upper bound less than lower bound)"
                  Nothing                 -> return (Q (Quantifier prim 1 (Upper 1) Greedy))

{- parseQuantifier': never fails *with* consuming input
 -}
parseQuantifier' :: Parsec String () (LowerBound, UpperBound, LazyOrGreedy)
parseQuantifier' = do 
    (lower, upper) <- choice [ (\_ -> (1, Unlimited)) <$> char '+'
                             , (\_ -> (0, Unlimited)) <$> char '*'
                             , (\_ -> (0, (Upper 1))) <$> char '?'
                             , try parseRange -- dont consume input upon failure here
                             ]
    lazyOrGreedy <- option Greedy $ (\_ -> Lazy) <$> char '?'
    return ( lower
           , upper
           , lazyOrGreedy
           )

parseRange :: Parsec String () (LowerBound, UpperBound)
parseRange = do
    char '{'
    lower <- try parseInt 
    n <- char ',' <|> char '}'
    if n == '}'
        then return (lower, Upper lower)
        else do
            upper <- optionMaybe . try $ parseInt
            char '}'
            case upper of Nothing -> return (lower, Unlimited)
                          Just u  -> return (lower, Upper u  )
    where parseInt = (read :: String -> LowerBound) <$> many1 digit

{- trimFat: 
 - remove redundant nodes, i.e. a union of one thing, 
 - quantified once or zero times, etc. 
 -}
trimFat  :: RETree -> RETree
trimFat reTree = fromMaybe Epsilon $ trimFat' reTree

trimFat' :: RETree -> Maybe RETree
trimFat' (Symbol s) = Just (Symbol s)

-- keep capture groups
trimFat' (CaptureGroup reTree) = Just . CaptureGroup . trimFat $ reTree
-- keep capture groups
trimFat' (Q (Quantifier (CaptureGroup reTree) 0 (Upper 0) lG)) = 
    Just . (\ rT -> Q (Quantifier (CaptureGroup rT) 0 (Upper 0) lG)) . trimFat $ reTree
trimFat' (Q (Quantifier reTree 0 (Upper 0) _)) = Nothing
trimFat' (Q (Quantifier reTree 1 (Upper 1) _)) = trimFat' reTree
trimFat' (Q (Quantifier reTree lower upper lG)) = 
    (\x -> Q (Quantifier x lower upper lG)) <$> (trimFat' reTree)

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
-- instance Eq RETree where
--     Epsilon == Epsilon = True
--     Symbol a == Symbol b = a == b
--     Q (Quantifier reTreeA lA uA _) == Q (Quantifier reTreeB lB uB _) = 
--       reTreeA == reTreeB && lA == lB && uA == uB
--     Concat forestA == Concat forestB =
--       all (\(a,b) -> a == b) $ zip forestA forestB
--     Union forestA == Concat forestB =
--       all (\(a,b) -> a == b) $ zip forestA forestB
--     _ == _ = False -- need to explicitly include default case, otherwise get
--                    -- non-exhaustive patterns when running 'stack test'



{-** RUNREGEX-related functions **-}
{-************************************************************************************-}

reMatches_ :: RETree -> MatchProgress -> [MatchProgress]

reMatches_ Epsilon state = return state

reMatches_ (Union []) _ = []
reMatches_ (Union _) (MatchProgress _ "" _) = []
reMatches_ (Union (reTree:reForest)) state = 
    reMatches_ reTree state <> reMatches_ (Union reForest) state
    
reMatches_ (Concat []) state = return state
reMatches_ (Concat _) (MatchProgress _ "" _) = []
reMatches_ (Concat (reTree:reForest)) state =
    reMatches_ reTree state >>= reMatches_ (Concat reForest) 

reMatches_ (Q qfier@(Quantifier reTree _ _ _)) state = 
    reSearchQfied [] searcher qfier 0 state
  where
    searcher = reMatches_ reTree
  
reMatches_ (Symbol _) (MatchProgress _ "" _) = []
reMatches_ (Symbol s) (MatchProgress consumed (r:remaining) groups)
  | s == r = [MatchProgress (r:consumed) remaining groups]
  | otherwise = []

reSearch (CaptureGroup reTree) state@(MatchProgress consumed "" groups) = return state
reSearch (CaptureGroup reTree) state@(MatchProgress consumed remaining groups) = 
    ( \(MatchProgress c r g) -> MatchProgress c r [MatchProgress c r g] ) <$> 
      reSearch reTree state

-- TODO: make Quantifier contain quantifier information only. not RegexRepr child.
reSearchQfied 
    :: [MatchProgress]                    
    -> (MatchProgress -> [MatchProgress]) 
    -> Quantifier
    -> Int                                
    -> (MatchProgress -> [MatchProgress])

-- no more parsing can be done, so return accumulated states.
reSearchQfied acc _ _ _ (MatchProgress _ "" _) = acc

reSearchQfied acc f qfier@(Quantifier _ lower upper lG) i state
    | nextStates == [] = acc
    | Upper i >  upper     = undefined -- should never be called with i > upper
    | Upper i == upper     = acc -- reached the max number of repetitions, now return the collection of states
    |       i >= lower - 1 = nextStates >>= reSearchQfied lazyOrGreedyUpdate f qfier (i+1) -- accumulate next states
    | otherwise      = nextStates >>= reSearchQfied acc f qfier (i+1) -- parse another quantified but don't accumulate
  where
    lazyOrGreedyUpdate = case lG of
      Lazy -> acc <> nextStates
      Greedy -> nextStates <> acc
    nextStates = f state

{- | ~=~: "extensional equivalence"
   a ~=~ b is true iff when applied to a higher order function (i.e. reMatches_), a and b 
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
    ptContents (CaptureGroup _) = "()"
    ptContents sym@(Symbol _) = unParse 0 sym
    ptContents (Q (Quantifier _ l (Upper u) _))
      | l == 0 && u == 1 = "?"
      | l == u           = "{" ++ show l ++ "}"
      | otherwise        = "{" ++ show l ++ "," ++ show u ++ "}"
    ptContents (Q (Quantifier _ l Unlimited _)) 
      | l == 0 = "(*)"
      | l == 1 = "(+)"
      | otherwise = "{" ++ show l ++ ",}"
    ptContents (Concat _) = "(++)"
    ptContents (Union  _) = "(|)"

    ptForest Epsilon = []
    ptForest (CaptureGroup reTree) = [reTree]
    ptForest (Symbol _) = []
    ptForest (Q (Quantifier reTree _ _ _)) = [reTree]
    ptForest (Concat reTrees) = reTrees
    ptForest (Union  reTrees) = reTrees

{- unparseTree :: exactly what you would expect :)
-}
unparseTree :: RETree -> String
unparseTree reTree = "/" ++ unParse 0 reTree ++ "/"
{- unParse: convert this regex tree to is string-regex form, making sure
   to place it in brackets to show precedence if its parent has higher-or-equal 
   precedence 
-}

unParse :: Int -> RETree -> String
unParse _ Epsilon = "\x03f5"

unParse _ (Symbol c) = [c]

unParse i (Q (Quantifier reTree lower Unlimited _))
  | lower == 1 = brkts $ ps reTree ++ "+"
  | lower == 0 = brkts $ ps reTree ++ "*"
  | otherwise  = brkts $ ps reTree ++ "{" ++ show lower ++ ",}"
  where brkts = brackets i 3
        ps    = unParse 3
unParse i (Q (Quantifier reTree lower (Upper upper) _))
  | lower == upper = brkts $ ps reTree ++ "{" ++ show lower ++ "}"
  | otherwise = brkts $ ps reTree ++ "{" ++ show lower ++ "," ++ show upper ++ "}"
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
