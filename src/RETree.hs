{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module RETree -- where
    ( RETree (..)
    , ChC (..)
    , Quantifier (..)
    , LowerBound
    , UpperBound (..)
    , LazyOrGreedy (..)
    , unparseTree
    ) where

import Text.Parsec
import Text.Parsec.String
import Data.Char (ord)
import Data.List (intersperse, stripPrefix)
import Data.Maybe (catMaybes, fromMaybe, fromJust, maybeToList)
import Data.Either (isLeft)
import Text.Printf (printf)

import TreePrint
import Regex

-- highest precedence parsing at the top
data RETree = 
    Epsilon -- the empty regular expression. matches everything
  | CaptureTree RETree
  | CharClass ChC
  | Symbol Char
  | Q Quantifier 
  | Concat [RETree]
  | Union [RETree]
  deriving (Eq, Read)

data ChC = 
    UnionChC [ChC] -- Same as RETree Union. ex: [\wa-z0-9ABC]
  | RangeChC Char Char -- Match a character in the unicode range denoted by these characters. ex: [a-z]
  | NotChC ChC -- inverted character class. examples: [^a-z] [^\w] \W
  deriving (Eq, Read)

-- Quantified regex's have their own special complexity so are defined as a seperate type
data Quantifier = Quantifier RETree LowerBound UpperBound LazyOrGreedy deriving (Eq, Read)
type LowerBound = Int
data UpperBound = Unlimited | Upper Int deriving (Read, Show, Eq)

instance Show Quantifier where
    show (Quantifier _ lower upper lG) = handleLazy lG $ case (lower, upper) of
        (0, Upper 1) -> "?"
        (0, Unlimited) -> "*"
        (1, Unlimited) -> "+"
        (l, Unlimited) -> printf "{%d,}" l
        (l, Upper u) -> printf "{%d,%d}" l u
      where 
        handleLazy Lazy str = str <> "?"
        handleLazy Greedy str = str



instance Ord UpperBound where
    _ <= Unlimited = True
    Upper r <= Upper l = r <= l

data LazyOrGreedy = Lazy | Greedy deriving (Eq, Read)

instance RegexRepr RETree where
    reCompile regex = (trimFat <$>) . parse (parseUnion <* eof) "" $ regex
    reMatches reTree input = reMatches_ reTree (MatchProgress "" input [])


{-** COMPILE functions & data **-}
{-************************************************************************************-}
type TreeParser = REParser RETree
{- parsePrimary, parseCaptureGroup, parseCharClass, parseQuantifier, parseConcat, and parseUnion: 
   a typical recursive-descent parser, presented in bottom-up order. -}
                       
parseUnion :: TreeParser
parseUnion = Union <$> sepBy parseConcat (char '|')

parseConcat :: TreeParser
parseConcat = Concat <$> many parseQuantifier

parsePrimary :: TreeParser
parsePrimary = choice $ try <$> 
    [ CaptureTree <$> ( (char '(') *> parseUnion <* (char ')') )
    , Symbol <$> (char '\\' *> oneOf metaChars)
    , Symbol <$> parseSymbolToken metaChars
    , parseCharClass
    ]

parseCharClass :: TreeParser
parseCharClass = choice [ CharClass <$> try ((char '[') *> parseInnerClass <* (char ']'))
                        , CharClass <$> parseClassToken
                        ]

{-** ChC parser **-}
-- parseInnerClass: calls parseUnionChC
-- parseUnionChC: calls parseRangeChC, parseSymbolToken, parseClassToken
-- parseRangeChc: calls parseSymbolToken, invalidBoundTokenRange
-- invalidBoundTokenRange
-- parseClassToken: calls lookupClass
-- lookupClass
{-************************************************************************************-}
parseInnerClass :: Parsec String () ChC
parseInnerClass = do
    inverted <- optionMaybe $ char '^'
    chClass <- parseUnionChC
    case chClass of
        UnionChC [] -> fail "parsed illegal empty character class"
        cls -> return $ case inverted of
                            Just _  -> NotChC cls
                            Nothing -> cls


parseUnionChC :: Parsec String () ChC
parseUnionChC = do
    -- lookahead to check that both <a> and <b> in a sequence "<a>-<b>" are symbol tokens:
    notFollowedBy invalidBoundTokenRange 
    maybeCls <- optionMaybe . choice $ try <$> [ parseRangeChC
                                               , ((\ c -> RangeChC c c) <$> parseSymbolToken classMetaChars)
                                               , parseClassToken
                                               ]
    case maybeCls of
        Nothing -> return $ UnionChC []
        Just cls -> (\(UnionChC cz) -> UnionChC (cls : cz)) <$> parseUnionChC

parseRangeChC :: Parsec String () ChC
parseRangeChC = do
    left  <- parseSymbolToken classMetaChars
    char '-'
    right <- parseSymbolToken classMetaChars
    if ord left > ord right
        then fail $ printf "parsed invalid character range: %c-%c" left right
        else return (RangeChC left right)

-- | checks for the case where a char range contains a character-class standin literal as lower or upper bound
-- hacky and kind of inefficient
invalidBoundTokenRange :: Parsec String () ()
invalidBoundTokenRange = do
    choice $ try <$> [ parseClassToken >> char '-' >> parseSymbolToken classMetaChars >> dummyChC
                     , parseSymbolToken classMetaChars >> char '-' >> parseClassToken
                     , parseClassToken >> char '-' >> parseClassToken
                     ]
    return ()
  where
    dummyChC = return $ RangeChC '\x0' '\x0' -- dummy value to comply with types

parseClassToken :: Parsec String () ChC
parseClassToken = lookupClass <$> (char '\\' *> oneOf classChars)

lookupClass :: Char -> ChC
lookupClass c = case c of
    'w' -> UnionChC [ (RangeChC 'a' 'z')
                    , (RangeChC 'A' 'Z')
                    , (RangeChC '0' '9')
                    , (RangeChC '_' '_')
                    ]
    'W' -> NotChC $ lookupClass 'w'
    _  -> undefined


{-************************************************************************************-}

parseSymbolToken :: [Char] -> Parsec String () Char
-- | parseSymbolToken: this should try to parse a literal symbol, 
-- escaped standin for a different character (i.e. \n, \a etc.)
-- or other escaped character. 
-- 'metaChars' argument specifies which set of characters are treated as 
-- meta-characters in this parsing context (its different inside character-classes)
parseSymbolToken metaChars = choice [ try $ noneOf metaChars
                                    , try $ parseEscapedStandin
                                    , char '\\' *> oneOf metaChars
                                    ]

parseEscapedStandin :: Parsec String () Char
parseEscapedStandin = lookupStandin <$> (char '\\' *> oneOf escapedStandinChars)

parseQuantifier :: TreeParser
parseQuantifier = do
    prim  <- parsePrimary
    range <- optionMaybe parseQuantifier'
    case range of Just (lower, upper, lG) 
                    | (Upper lower) <= upper -> return (Q (Quantifier prim lower upper lG))
                    | otherwise -> fail $ "parsed an invalid range: " <> show (Quantifier Epsilon lower upper lG)
                  Nothing                 -> return (Q (Quantifier prim 1 (Upper 1) Greedy))

{- parseQuantifier': only fails without consuming input
 -}
parseQuantifier' :: Parsec String () (LowerBound, UpperBound, LazyOrGreedy)
parseQuantifier' = do 
    (lower, upper) <- choice [ (\_ -> (1, Unlimited)) <$> char '+'
                             , (\_ -> (0, Unlimited)) <$> char '*'
                             , (\_ -> (0, (Upper 1))) <$> char '?'
                             , try parseRange -- dont consume input upon failure here either
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
trimFat' (CaptureTree reTree) = Just . CaptureTree . trimFat $ reTree

trimFat' (CharClass chC) = do
    chC <- trimFatChC chC
    Just . CharClass $ chC

-- keep capture groups
trimFat' (Q (Quantifier (CaptureTree reTree) 0 (Upper 0) lG)) = 
    Just . (\ rT -> Q (Quantifier (CaptureTree rT) 0 (Upper 0) lG)) . trimFat $ reTree
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

trimFatChC :: ChC -> Maybe ChC
trimFatChC (UnionChC []) = undefined -- shouldnt be able to parse an empty ChC in the first place
trimFatChC (UnionChC [ccTree]) = trimFatChC ccTree
trimFatChC (UnionChC ccForest) = case catMaybes $ trimFatChC <$> ccForest of
    [] -> Nothing
    ccForest' -> Just . UnionChC $ ccForest'

trimFatChC (NotChC ccTree) = do
    ccTree' <- trimFatChC ccTree
    Just . NotChC $ ccTree'

trimFatChC r = Just r


{-** RUNREGEX functions **-}
{-************************************************************************************-}

reMatches_ :: RETree -> MatchProgress -> [MatchProgress]
-- Epsilon matches everything
reMatches_ Epsilon state@(MatchProgress consumed "" groups) = [state]
reMatches_ Epsilon state@(MatchProgress consumed (r:remaining) groups) = 
  state : ( reMatches_ Epsilon (MatchProgress (r:consumed) remaining groups) )

reMatches_ (Union []) _ = []
-- FALSE: you can match nothing with quantifiers -- reMatches_ (Union _) (MatchProgress _ "" _) = []
reMatches_ (Union (reTree:reForest)) state = 
    reMatches_ reTree state <> reMatches_ (Union reForest) state
    
reMatches_ (Concat []) state = return state
-- FALSE: you can match nothing with quantifiers -- reMatches_ (Concat _) (MatchProgress _ "" _) = []
reMatches_ (Concat (reTree:reForest)) state =
    reMatches_ reTree state >>= reMatches_ (Concat reForest) 

reMatches_ (Q qfier@(Quantifier reTree lower _ lG)) state
    | lower == 0 = lazyOrGreedyZeroMatch $ reMatchesQfied searcher qfier 0 state -- see precondition of reMatchesQfied
    | otherwise  = reMatchesQfied searcher qfier 0 state
  where
    lazyOrGreedyZeroMatch = case lG of
      Lazy   -> ([state] <>)
      Greedy -> (<> [state])
    searcher = reMatches_ reTree
  
reMatches_ (Symbol _) (MatchProgress _ "" _) = []
reMatches_ (Symbol s) (MatchProgress consumed (r:remaining) groups)
  | s == r = [MatchProgress (r:consumed) remaining groups]
  | otherwise = []

-- FALSE: you can match nothing with quantifiers -- reMatches_ (CaptureTree reTree) state@(MatchProgress consumed "" groups) = return state
reMatches_ (CaptureTree reTree) state@(MatchProgress consumed remaining groups) =
    nxtStatesWithCapture
  where
    -- step 1: compute the next states with empty group counter
    -- notice that the groups accumulator is set to empty
    nxtStates = reMatches_ reTree (MatchProgress consumed remaining [])
    -- step 2: for each next state, determine what was captured and store it in the
    -- correct position in the groups field
    nxtStatesWithCapture = recordGroup <$> nxtStates
    recordGroup (MatchProgress nxtConsumed r nxtGroups) =
        MatchProgress 
          nxtConsumed 
          r 
        -- get just the difference i.e. just the string consumed in this capture group
        -- and insert it *before* the succeeding groups, but *after* the groups captured so
        -- far.
          (  groups
          ++ [CaptureGroup . fromJust . stripPrefix (reverse consumed) $ reverse nxtConsumed]
          ++ nxtGroups
          )

reMatches_ (CharClass chC) state = maybeToList $ reMatchesChC chC state

-- TODO: make Quantifier contain quantifier information only. not RegexRepr child.
{- reMatchesQfied: 
    PRECONDITION: THIS DOES NOT HANDLE MATCHING OF ZERO REPITITIONS! 
                  must init accumulator with starting state if quantifier accepts zero matches
-}
reMatchesQfied 
    :: (MatchProgress -> [MatchProgress]) 
    -> Quantifier
    -> Int                                
    -> (MatchProgress -> [MatchProgress])

-- no more parsing can be done to add new *unique* states, so return accumulated states.
reMatchesQfied _ _ _ (MatchProgress _ "" _) = []

reMatchesQfied f qfier@(Quantifier _ lower upper lG) i state
    | nextStates == [] = []
    | Upper i >  upper     = undefined -- should never be called with i > upper
    | Upper i == upper     = [] -- reached the max number of repetitions
    |       i >= lower - 1 = lazyOrGreedyUpdate $ nextStates >>= reMatchesQfied f qfier (i+1) -- accumulate next states
    | otherwise            = nextStates >>= reMatchesQfied f qfier (i+1) -- parse another quantified but don't accumulate
  where
    lazyOrGreedyUpdate = case lG of
      Lazy   -> (nextStates <>)
      Greedy -> (<> nextStates)
    nextStates = f state

reMatchesChC :: ChC -> MatchProgress -> Maybe MatchProgress
-- there must be at least one char for a character class to parse:
reMatchesChC _ (MatchProgress _ "" _) = Nothing

reMatchesChC (RangeChC lower upper) (MatchProgress consumed (c:remaining) groups)
    | ord lower <= ord c && ord c <= ord upper = Just (MatchProgress (c:consumed) remaining groups)
    | otherwise = Nothing

reMatchesChC (NotChC ccTree) mP@(MatchProgress consumed (c:remaining) groups) = case reMatchesChC ccTree mP of
    Just _  -> Nothing
    Nothing -> Just (MatchProgress (c:consumed) remaining groups)

reMatchesChC (UnionChC []) _ = Nothing
reMatchesChC (UnionChC (cT:ccForest)) mP = case reMatchesChC cT mP of
    Nothing -> reMatchesChC (UnionChC ccForest) mP
    Just mP -> Just mP

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



{-** SHOW functions **-}
{-************************************************************************************-}
instance Show RETree where
    show = ptShow -- for instsances of PrintableTree

instance PrintableTree RETree where
    ptContents Epsilon = unParse 0 Epsilon
    ptContents (CaptureTree _) = "()"
    ptContents (CharClass cc) = ptContents cc
    ptContents sym@(Symbol _) = unParse 0 sym
    ptContents (Q (Quantifier _ l (Upper u) _))
      | l == 0 && u == 1 = "?"
      | l == u           = "{" ++ show l ++ "}"
      | otherwise        = "{" ++ show l ++ "," ++ show u ++ "}"
    ptContents (Q (Quantifier _ l Unlimited _)) 
      | l == 0 = "(*)"
      | l == 1 = "(+)"
      | otherwise = "{" ++ show l ++ ",}"
    ptContents (Concat _) = "(<>)"
    ptContents (Union  _) = "(|)"

    ptForest Epsilon = []
    ptForest (CaptureTree reTree) = [reTree]
    ptForest (CharClass chC) = CharClass <$> ptForest chC
    ptForest (Symbol _) = []
    ptForest (Q (Quantifier reTree _ _ _)) = [reTree]
    ptForest (Concat reTrees) = reTrees
    ptForest (Union  reTrees) = reTrees

instance Show ChC where
    show = ptShow

instance PrintableTree ChC where
    ptContents (UnionChC _) = "[|]"
    ptContents (NotChC ccTree) = "[^]"
    ptContents (RangeChC lower upper)
        | lower == upper = printf "[%c]" lower
        | otherwise      = printf "[%c-%c]" lower upper
    ptForest (RangeChC _ _) = []
    ptForest (UnionChC ccForest) = ccForest
    ptForest (NotChC ccTree) = [ccTree]

{- unparseTree :: exactly what you would expect :)
-}
unparseTree :: RETree -> String
unparseTree reTree = "/" ++ unParse 0 reTree ++ "/"
{- unParse: convert this regex tree to is string-regex form, making sure
   to place it in brackets to show precedence if its parent has higher-or-equal 
   precedence 
-}

-- TODO: implement for newly added data constructors
-- TODO: add a function precedence :: RETree -> Int
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
