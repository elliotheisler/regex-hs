{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module TreePrint where

import Data.List (intersperse, intercalate)

class (Show a) => PrintableTree t a | t -> a where
    nodeContents :: t -> a
    getLeftRight :: t -> ([t], [t])
    getLeftForest :: t -> [t]
    getRightForest :: t -> [t]
    getLeftRight t = (getLeftForest t, getRightForest t)
    getLeftForest  = fst . getLeftRight
    getRightForest = snd . getLeftRight

-- ExampleTree data, left branches, and right branches
data ExampleTree    = ExampleTree  String [ExampleTree] [ExampleTree]
data ExampleTree2 a = ExampleTree2 a [ExampleTree2 a] [ExampleTree2 a]

instance PrintableTree ExampleTree String where
    nodeContents (ExampleTree s _ _) = s
    getLeftRight (ExampleTree s lTrees rTrees) = (lTrees, rTrees)
-- NOTE: the '(Show a) =>' is necessary in both the class and the instance
instance (Show a) => PrintableTree (ExampleTree2 a) a where
    nodeContents (ExampleTree2 s _ _) = s
    getLeftRight (ExampleTree2 s lTrees rTrees) = (lTrees, rTrees)

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

treeShow :: (PrintableTree t a) => t -> String
treeShow = toStr . (\ (t,m,b) -> t++m++b) . treeShow'
  where toStr = intercalate "\n"

type TopMidBot = (CharGrid, CharGrid, CharGrid)
data TMB = Top | Middle | Bottom

treeShow' :: (PrintableTree t a) => t -> TopMidBot
treeShow' t =
    ( padLefts lChildren
    , [contents ++ getRoot lTrees rTrees]
    , padRights rChildren
    )
  where
    (lTrees, rTrees) = getLeftRight t
    contents = show . nodeContents $ t
    lChildren = treeShow' <$> lTrees :: [TopMidBot]
    rChildren = treeShow' <$> rTrees :: [TopMidBot]

    padLefts  = map (wSpace++) . concatPadBranches Top    addBranchMid addBranchTop
    padRights = map (wSpace++) . concatPadBranches Bottom addBranchMid addBranchBot
    wSpace = replicate (length contents) ' '
    concatPadBranches :: TMB
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

    addBranchMid = addBranchChars arm armBranch arm
    addBranchTop = addBranchChars ' ' cornerTop arm
    addBranchBot = addBranchChars arm cornerBot ' '
    addBranchChars :: Char -> Char -> Char -> (TopMidBot -> TopMidBot)
    addBranchChars topChar midChar botChar (t,m,b) =
      ( (topChar:) <$> t
      , (midChar:) <$> m
      , (botChar:) <$> b
      )

    cornerTop = '\x250c'
    cornerBot = '\x2514'
    arm       = '\x2502'
    armBranch = '\x251c'

    getRoot :: (PrintableTree t a) => [t] -> [t] -> String
    getRoot [] [] = " "
    getRoot [] r  = "\x2510"
    getRoot l  [] = "\x2518"
    getRoot l  r  = "\x2524"


-- main :: IO ()
-- main = do
--     let b = ExampleTree "b" [(ExampleTree "br" [] [])] []
--     let c = ExampleTree "cee" [(ExampleTree "cl" [] []), (ExampleTree "cll" [] [])] [(ExampleTree "cr" [] [])]
--     let t = ExampleTree "R" 
--             [b, (ExampleTree "aaaaa" [] [])] 
--             [c, b]
--     putLong c
--     putLong t
