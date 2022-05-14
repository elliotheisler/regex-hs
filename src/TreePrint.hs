module TreePrint where

import Data.List (intersperse, intercalate)

-- tree data, left branches, and right branches
data Tree = Tree String [Tree] [Tree]
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

wideShow :: Tree -> CharGrid
wideShow (Tree s lTrees rTrees) =
    [whiteSpL `sep` s `sep` whiteSpR] ++
    trConcat [leftRepr, [replicate (length s) ' '], rightRepr]
  where
    leftRepr  = trConcat $ wideShow <$> lTrees
    rightRepr = trConcat $ wideShow <$> rTrees
    whiteSpL  = replicate (width leftRepr ) ' '
    whiteSpR  = replicate (width rightRepr) ' '

putWide = putStrLn . prettyStrRepr . wideShow

type TopMidBot = (CharGrid, CharGrid, CharGrid)

longShow :: Tree -> TopMidBot
longShow (Tree s lTrees rTrees) =
    ( padLefts lChildren
    , [s ++ getRoot lTrees rTrees]
    , padRights rChildren
    )
  where
    lChildren = longShow <$> lTrees :: [TopMidBot]
    rChildren = longShow <$> rTrees :: [TopMidBot]
    wSpace = replicate (length s) ' '

    padLefts  = map (wSpace++) . concatPadBranches addBranchTop addBranchMid addBranchMid
    padRights = map (wSpace++) . concatPadBranches addBranchMid addBranchMid addBranchBot
    concatPadBranches :: (TopMidBot -> TopMidBot) 
                      -> (TopMidBot -> TopMidBot) 
                      -> (TopMidBot -> TopMidBot) 
                      -> [TopMidBot] 
                      -> CharGrid
    concatPadBranches _ _ _ [] = []
    concatPadBranches topCase midCase botCase (h:tail) =
      let (body,last) = splitAt (length tail - 1) tail -- we now have the head, body, and last
          padded       = [topCase h] ++ (midCase <$> body) ++ (botCase <$> last) :: [TopMidBot]
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

    getRoot :: [Tree] -> [Tree] -> String
    getRoot [] [] = " "
    getRoot [] r  = "\x2510"
    getRoot l  [] = "\x2518"
    getRoot l  r  = "\x2524"
    
    

putLong = putStrLn . prettyStrRepr . (\ (t,m,b) -> t++m++b) . longShow
    

prettyStrRepr = intercalate "\n"

instance Show Tree where
    show = prettyStrRepr . wideShow

main :: IO ()
main = do
    let t = Tree "R" [(Tree "a" [] []), (Tree "b" [] [])] [(Tree "c" [] []), (Tree "d" [(Tree "f" [] [])] [])]
    putLong t
