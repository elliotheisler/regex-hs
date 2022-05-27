{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module TreePrint where


import Data.List  (intercalate, null)
import Data.Maybe (catMaybes, isNothing, maybeToList, listToMaybe, fromMaybe)
import Data.Bool  (bool)


class (Show a) => PrintableTree t a | t -> a where
  {- minimal definition: 
     nodeContents & getForest | getLeftMiddleRight | (getLeftForest & getMiddleForest & getRightForest)
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
treeShow = toStr . toGridStack . treeShow'
  where 
    toGridStack :: TopMidBot -> CharGrid
    toGridStack (t,m,b) = t++m++b
    toStr :: CharGrid -> String
    toStr = intercalate "\n"

type CharGrid = [String]

type TopMidBot = (CharGrid, CharGrid, CharGrid)

-- this function and its sub-functions are the bulk of this module
treeShow' :: (PrintableTree tree a) => tree -> TopMidBot
treeShow' tree = 
    let (midTop, midMid, midBot) = padMiddle mChild
    in  ( padLefts lChildren ++ midTop
        , midMid
        , midBot ++ padRights rChildren
        )
  where
    contents = nodeStrContents tree
    wSpace   = replicate (length contents) ' '

    (lTrees, mTree, rTrees) = getLeftMiddleRight tree

    lChildren = treeShow' <$> lTrees :: [TopMidBot]
    mChild    = maybe ([],[""],[]) treeShow' mTree  :: TopMidBot
    rChildren = treeShow' <$> rTrees :: [TopMidBot]

    padLefts  :: [TopMidBot] -> CharGrid
    padLefts l  = 
      concatPadBranches Top addBranchTopMid addBranchTopTop $ l

    padRights :: [TopMidBot] -> CharGrid
    padRights r = 
      concatPadBranches Bottom addBranchBotMid addBranchBotBot $ r

{- case for middle tree more complicated than for top and bottom forests, 
 - so i define it all right here -}
padMiddle :: TopMidBot -> TopMidBot
padMiddle m = addPrefixes midTopChar midMidChar midBotChar m
  where
    midTopChar = (wSpace++  ) . pure . bC $ if null lChildren 
                                            then []
                                            else [N,S]
    midBotChar = (wSpace++  ) . pure . bC $ if null rChildren 
                                            then [] 
                                            else [N,S]
    midMidChar = (contents++) . pure . bC . catMaybes $
      [ if null   lTrees then Nothing else Just N
      , if isNothing mTree then Nothing else Just E
      , if null   rTrees then Nothing else Just S
      , if null lTrees && isNothing mTree && null rTrees 
        then Nothing 
        else Just W
      ]

data TopOrBottom = Top | Bottom

concatPadBranches :: TopOrBottom
                  -> (TopMidBot -> TopMidBot) 
                  -> (TopMidBot -> TopMidBot) 
                  -> [TopMidBot] 
                  -> CharGrid
concatPadBranches _ _ _ [] = []
concatPadBranches topOrBot midCase endCase sections@(h:tail) = 
    let padded = case topOrBot of
                 Top    -> [endCase h] ++ (midCase <$> tail)
                 Bottom -> let (init, (l:_)) = splitAt (length sections - 1) sections
                           in  (midCase <$> init) ++ [endCase l]
    in  concat $ (\ (t,m,b) -> t++m++b) <$> padded :: CharGrid

-- cases for top/left forest
addBranchTopMid = addPrefixes (wSpace ++ [bC [N,S]]) (wSpace ++ [bC [N,S,E]]) (wSpace ++ [bC [N,S]])
addBranchTopTop = addPrefixes (wSpace ++ [bC []]) (wSpace ++ [bC [S,E]]) (wSpace ++ [bC [N,S]])
addBranchTopBot = addBranchTopMid
-- cases for bottom/right forest
addBranchBotMid = addBranchTopMid
addBranchBotTop = addBranchTopMid
addBranchBotBot = addPrefixes (wSpace ++ [bC [N,S]]) (wSpace ++ [bC [N,E]]) (wSpace ++ [bC []])

addPrefixes :: String -> String -> String -> (TopMidBot -> TopMidBot)
addPrefixes topPfix midPfix botPfix (t,m,b) =
  ( (topPfix++) <$> t
  , (midPfix++) <$> m
  , (botPfix++) <$> b
  )

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

