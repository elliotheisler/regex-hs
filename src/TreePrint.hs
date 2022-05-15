{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module TreePrint where

import Data.List (intersperse, intercalate, null)
import Data.Maybe (catMaybes, isNothing, maybeToList, listToMaybe, fromMaybe)
import Data.Bool  (bool)

class (Show a) => PrintableTree t a | t -> a where
  {- minimal definition: 
     nodeContents & getForest | getLeftMiddleRight | (getLeftForest & getMiddleForest & getRightForest)
  -}
    nodeContents :: t -> a
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

-- ExampleTree data, left branches, and right branches
data ExampleTree    = ExampleTree  String [ExampleTree] [ExampleTree]
data ExampleTree2 a = ExampleTree2 a [ExampleTree2 a] [ExampleTree2 a]

instance PrintableTree ExampleTree String where
    nodeContents (ExampleTree s _ _) = s
    getLeftMiddleRight (ExampleTree s lTrees rTrees) = (lTrees, mTree, rTrees)
      where
        (maybeMiddle,rTree) = splitAt 0 rTrees
        mTree = listToMaybe maybeMiddle
-- NOTE: the '(Show a) =>' is necessary in both the class and the instance
instance (Show a) => PrintableTree (ExampleTree2 a) a where
    nodeContents (ExampleTree2 s _ _) = s
    getLeftMiddleRight (ExampleTree2 s lTrees rTrees) = (lTrees, mTree, rTrees)
      where
        (maybeMiddle,rTree) = splitAt 0 rTrees
        mTree = listToMaybe maybeMiddle

type CharGrid = [String]

-- TODO: could make tail-recursive
zipWithAll :: (l -> r -> c) -> (l -> c) -> (r -> c) -> [l] -> [r] -> [c]
zipWithAll f fl fr [] [] = []
zipWithAll f fl fr (h:t) [] = (fl h) : zipWithAll f fl fr t []
zipWithAll f fl fr [] (h:t) = (fr h) : zipWithAll f fl fr [] t
zipWithAll f fl fr (hl:tl) (hr:tr) = f hl hr : zipWithAll f fl fr tl tr

width :: CharGrid -> Int
width []      = 0
width strRepr = length . head $ strRepr

infixr 5 `sep`
-- sep = (++) -- use this to print without the pipe characters
sep l  "" = l
sep "" r  = r
sep l  r  = l ++ "|" ++ r

trAppend :: CharGrid -> CharGrid -> CharGrid
trAppend l [] = l
trAppend [] r = r
trAppend l r = zipWithAll
               (sep)
               (`sep` replicate (width r) ' ')
               (replicate (width l) ' ' `sep`) 
               l
               r

trConcat :: [CharGrid] -> CharGrid
trConcat [] = []
trConcat l  = foldl1 trAppend l

wideShow :: ExampleTree -> CharGrid
wideShow (ExampleTree s lTrees rTrees) =
    [whiteSpL `sep` s `sep` whiteSpR] ++
    trConcat [leftRepr, [replicate (length s) ' '], rightRepr]
  where
    leftRepr  = trConcat $ wideShow <$> lTrees
    rightRepr = trConcat $ wideShow <$> rTrees
    whiteSpL  = replicate (width leftRepr ) ' '
    whiteSpR  = replicate (width rightRepr) ' '

data Adjacency = N | S | E | W deriving (Eq)

{- "boxChar": given a list of adjacency values, 
    get the corresponding unicode box-drawing character.
    - order shouldn't matter the way i ordered the guards
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

treeShow :: (PrintableTree t a) => t -> String
treeShow = toStr . toGridStack . treeShow'
  where 
    toStr :: CharGrid -> String
    toStr = intercalate "\n"
    toGridStack :: TopMidBot -> CharGrid
    toGridStack (t,m,b) = t++m++b

type TopMidBot = (CharGrid, CharGrid, CharGrid)

data TMB = Top | Middle | Bottom

treeShow' :: (PrintableTree tree a) => tree -> TopMidBot
treeShow' tree =
    ( padLefts  lChildren
    , padMiddle mChild
    , padRights rChildren
    )
  where
    (lTrees, mTree, rTrees) = getLeftMiddleRight tree

    lChildren = treeShow' <$> lTrees :: [TopMidBot]
    mChild    = treeShow' <$> mTree  :: Maybe TopMidBot
    rChildren = treeShow' <$> rTrees :: [TopMidBot]

    contents = show . nodeContents $ tree
    wSpace   = replicate (length contents) ' '

    padLefts l  = 
      map (wSpace++) . concatPadBranches Top addBranchTopMid addBranchTopTop $ l
    padMiddle m = 
        map (contents++) . concatPadBranches Middle allCase allCase . pure . fromMaybe ([],[""],[]) $ m
      where 
        allCase = addBranchChars midTopChar midMidChar midBotChar
          where
            midTopChar = bC $ if null lChildren 
                              then [] :: [Adjacency]
                              else [N,S]
            midMidChar = bC . catMaybes $
              [ if null   lChildren then Nothing else Just N
              , if isNothing mChild    then Nothing else Just E
              , if null   rChildren then Nothing else Just S
              , if null lChildren && isNothing mChild && null rChildren 
                then Nothing 
                else Just W
              ]
            midBotChar = bC $ if null rChildren 
                              then [] 
                              else [N,S]
    padRights r = 
      map (wSpace++) . concatPadBranches Bottom addBranchBotMid addBranchBotBot $ r


    concatPadBranches :: TMB
                      -> (TopMidBot -> TopMidBot) 
                      -> (TopMidBot -> TopMidBot) 
                      -> [TopMidBot] 
                      -> CharGrid
    concatPadBranches _ _ _ [] = []
    concatPadBranches topOrBot midCase endCase sections@(h:tail) = 
        let padded = case topOrBot of
                     Top    -> [endCase h] ++ (midCase <$> tail)
                     Middle -> midCase <$> sections -- singleton list
                     Bottom -> let (init, (l:_)) = splitAt (length sections - 1) sections
                               in  (midCase <$> init) ++ [endCase l]
        in  concat $ (\ (tree,m,b) -> tree++m++b) <$> padded :: CharGrid

    -- cases for top/left forest
    addBranchTopMid = addBranchChars (bC [N,S]) (bC [N,S,E]) (bC [N,S])
    addBranchTopTop = addBranchChars (bC []) (bC [S,E]) (bC [N,S])
    addBranchTopBot = addBranchTopMid
    -- cases for bottom/right forest
    addBranchBotMid = addBranchTopMid
    addBranchBotTop = addBranchBotMid
    addBranchBotBot = addBranchChars (bC [N,S]) (bC [N,E]) (bC [])

    addBranchChars :: Char -> Char -> Char -> (TopMidBot -> TopMidBot)
    addBranchChars topChar midChar botChar (tree,m,b) =
      ( (topChar:) <$> tree
      , (midChar:) <$> m
      , (botChar:) <$> b
      )


main :: IO ()
main = do
    let a = ExampleTree "$" [] []
    let b = ExampleTree "b" [(ExampleTree "br" [] [])] []
    let c = ExampleTree "cee" [(ExampleTree "cl" [] []), (ExampleTree "cll" [] [])] [(ExampleTree "cr" [] [])]
    let tree = ExampleTree "R" 
            [b, (ExampleTree "aaaaa" [] [])] 
            [c, b]
    p a
    p c
    p tree
  where
    p :: ExampleTree -> IO ()
    p = putStrLn . treeShow
