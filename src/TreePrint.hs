-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE FlexibleInstances #-}
module TreePrint (
    PrintableTree (..)
  , ptShow
) where


import Data.List  (intercalate, null)
import Data.Maybe (catMaybes, isNothing, maybeToList, listToMaybe, fromMaybe)
import Data.Bool  (bool)


class PrintableTree t where
  {- minimal definition: 
     ptContents & (ptForest | ptTopMidBot | (ptTopForest & ptMidForest & ptBotForest))
  -}
    ptContents :: t -> String
    ptTopMidBot :: t -> ([t], Maybe t, [t])
    ptForest :: t -> [t]
    ptTopForest :: t -> [t]
    ptMidForest :: t -> Maybe t
    ptBotForest :: t -> [t]

    ptForest = (\ (l,m,r) -> l ++ (maybeToList m) ++ r) . ptTopMidBot

    ptTopMidBot t = 
      let trees = ptForest t
          (l,r)  = splitAt (length trees `div` 2) trees
          (m,r') = case r of []       -> (Nothing, r  )
                             (h:tail) -> (Just h, tail)
      in  (l,m,r')
    ptTopMidBot t = (ptTopForest t, ptMidForest t, ptBotForest t)

    ptTopForest   = (\ (l,_,_) -> l) . ptTopMidBot
    ptMidForest = (\ (_,m,_) -> m) . ptTopMidBot
    ptBotForest  = (\ (_,_,r) -> r) . ptTopMidBot

ptShow :: (PrintableTree tree) => tree -> String
ptShow = ("\n"++) . toStr . toGrid . ptShow' -- pad the top with a newline so it prints neatly within a Maybe, Either etc.
  where 
    toGrid :: TopMidBot -> CharGrid
    toGrid (t,m,b) = t++m++b
    toStr :: CharGrid -> String
    toStr = intercalate "\n"

type CharGrid = [String]

type TopMidBot = (CharGrid, CharGrid, CharGrid)

-- this function and its sub-functions make up the bulk of this module
ptShow' :: (PrintableTree tree) => tree -> TopMidBot
ptShow' tree = 
    (top, middle, bottom)
  where
    {- ~~~Step 1. convert left forest, middle child, and right forest into
     - TopMidBot representations -}
    (lTrees, mTree, rTrees) = ptTopMidBot tree

    lChildren = ptShow' <$> lTrees :: [TopMidBot]
    mChild    = maybe ([],[""],[]) ptShow' mTree  :: TopMidBot
    rChildren = ptShow' <$> rTrees :: [TopMidBot]

    contents = ptContents tree
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

