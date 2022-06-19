{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module TreePrint (
    PrintableTree (..)
  , treeShow
) where


import Data.List  (intercalate, null)
import Data.Maybe (catMaybes, isNothing, maybeToList, listToMaybe, fromMaybe)
import Data.Bool  (bool)


class (Show a) => PrintableTree t a | t -> a where
  {- minimal definition: 
     nodeContents & (getForest | getLeftMiddleRight | (getLeftForest & getMiddleForest & getRightForest))
  -}
    nodeContents :: t -> a
    nodeStrContents :: t -> String
    getLeftMiddleRight :: t -> ([t], Maybe t, [t])
    getForest :: t -> [t]
    getLeftForest :: t -> [t]
    getMiddleForest :: t -> Maybe t
    getRightForest :: t -> [t]


    getForest = (\ (l,m,r) -> l ++ (maybeToList m) ++ r) . getLeftMiddleRight
    getLeftMiddleRight t = 
      let trees = getForest t
          (l,r)  = splitAt (length trees `div` 2) trees
          (m,r') = case r of []       -> (Nothing, r  )
                             (h:tail) -> (Just h, tail)
      in  (l,m,r')
    getLeftMiddleRight t = (getLeftForest t, getMiddleForest t, getRightForest t)
    getLeftForest   = (\ (l,_,_) -> l) . getLeftMiddleRight
    getMiddleForest = (\ (_,m,_) -> m) . getLeftMiddleRight
    getRightForest  = (\ (_,_,r) -> r) . getLeftMiddleRight

    nodeStrContents = show . nodeContents

treeShow :: (PrintableTree tree a) => tree -> String
treeShow = ("\n"++) . toStr . toGrid . treeShow' -- pad the top with a newline so it prints neatly within a Maybe, Either etc.
  where 
    toGrid :: TopMidBot -> CharGrid
    toGrid (t,m,b) = t++m++b
    toStr :: CharGrid -> String
    toStr = intercalate "\n"

type CharGrid = [String]

type TopMidBot = (CharGrid, CharGrid, CharGrid)

-- this function and its sub-functions make up the bulk of this module
treeShow' :: (PrintableTree tree a) => tree -> TopMidBot
treeShow' tree = 
    (top, middle, bottom)
  where
    {- ~~~Step 1. convert left forest, middle child, and right forest into
     - TopMidBot representations -}
    (lTrees, mTree, rTrees) = getLeftMiddleRight tree

    lChildren = treeShow' <$> lTrees :: [TopMidBot]
    mChild    = maybe ([],[""],[]) treeShow' mTree  :: TopMidBot
    rChildren = treeShow' <$> rTrees :: [TopMidBot]

    contents = nodeStrContents tree
    wSpace   = replicate (length contents) ' '

    {- ~~~Step 2: pad top forest, middle tree, and bottom forest with box chars
       and whitespace (or this node's contents in the case of the middle row.
       core functions are addBoxChars and indent, respectively. -}
    padMiddle :: TopMidBot -> TopMidBot
    padMiddle = indentMiddle . addBoxChars midTopChar midMidChar midBotChar
      where
        midTopChar = bC $ if null lChildren 
                                                then []
                                                else [N,S]
        midBotChar = bC $ if null rChildren 
                                                then [] 
                                                else [N,S]
        midMidChar = bC . catMaybes $
          [ if null   lTrees then Nothing else Just N
          , if isNothing mTree then Nothing else Just E
          , if null   rTrees then Nothing else Just S
          , if null lTrees && isNothing mTree && null rTrees 
            then Nothing 
            else Just W
          ]
        indentMiddle = indent wSpace contents wSpace

    padTops :: [TopMidBot] -> [TopMidBot]
    padTops = map (indent wSpace wSpace wSpace) . bCharsTop

    bCharsTop :: [TopMidBot] -> [TopMidBot]
    bCharsTop [] = []
    bCharsTop (tree:forest) = [bCharsTopTop tree] ++ (bCharsTopMid <$> forest)

    padBottoms :: [TopMidBot] -> [TopMidBot]
    padBottoms = map (indent wSpace wSpace wSpace) . bCharsBottom

    bCharsBottom :: [TopMidBot] -> [TopMidBot]
    bCharsBottom [] = []
    bCharsBottom forest = 
      let (init, (l:_)) = splitAt (length forest - 1) forest
      in  (bCharsBotMid <$> init) ++ [bCharsBotBot l]
    
    indent :: String -> String -> String -> (TopMidBot -> TopMidBot)
    indent topSpace midSpace botSpace (t,m,b) = ( (topSpace++) <$> t
                                                , (midSpace++) <$> m
                                                , (botSpace++) <$> b
                                                )
    {- addBoxChars: 
     - functions for prefixing a tree-representation with box-drawing characters
     - representing the branches. Unused cases are commented out, but shown 
     - anyway to illustrate the idea. -}
 
    {- cases for top/left forest -}
    bCharsTopMid = addBoxChars (bC [N,S]) (bC [N,S,E]) (bC [N,S])
    bCharsTopTop = addBoxChars (bC []) (bC [S,E]) (bC [N,S])
 -- bCharsTopBot = bCharsTopMid
    {- cases for bottom/right forest -}
    bCharsBotMid = bCharsTopMid
 -- bCharsBotTop = bCharsTopMid
    bCharsBotBot = addBoxChars (bC [N,S]) (bC [N,E]) (bC [])

    addBoxChars :: Char -> Char -> Char -> (TopMidBot -> TopMidBot)
    addBoxChars topPfix midPfix botPfix (t,m,b) =
      ( (topPfix:) <$> t
      , (midPfix:) <$> m
      , (botPfix:) <$> b
      )

    {- ~~~Step 3: construct TopMidBot representation.
       note that the top and bottom of the *middle representation* 
       is concatenated onto top and bottom lists, respectively -}

    (midTop, midMid, midBot) = padMiddle mChild
    top = (concatReprs . padTops $ lChildren) ++ midTop
    middle = midMid
    bottom = midBot ++ (concatReprs . padBottoms $ rChildren)

    concatReprs :: [TopMidBot] -> CharGrid
    concatReprs = concat . map (\ (t,m,b) -> t++m++b)

data Adjacency = N | S | E | W deriving (Eq)

{- "boxChar": given a list of adjacency values, 
    get the corresponding unicode box-drawing character.
    - order shouldn't matter with the way i ordered the guards
-}
bC :: [Adjacency] -> Char
bC ls
    | ls `contains` [N,S,E,W] = '\x253c'
    | ls `contains` [  S,E,W] = '\x252c'
    | ls `contains` [N,  E,W] = '\x2534'
    | ls `contains` [N,S,  W] = '\x2524'
    | ls `contains` [N,S,E  ] = '\x251c'
    | ls `contains` [    E,W] = '\x2500'
    | ls `contains` [  S,  W] = '\x2510'
    | ls `contains` [N,    W] = '\x2518'
    | ls `contains` [  S,E  ] = '\x250c'
    | ls `contains` [N,  E  ] = '\x2514'
    | ls `contains` [N,S    ] = '\x2502'
    -- single-char cases unused
    | ls `contains` [      W] = '*'
    | ls `contains` [    E  ] = '*'
    | ls `contains` [  S    ] = '*'
    | ls `contains` [N      ] = '*'
    | ls `contains` [       ] = ' '
  where
    contains :: (Eq a) => [a] -> [a] -> Bool
    -- true iff every list element in bs occurs in as
    contains as bs = all (`elem` as) bs

