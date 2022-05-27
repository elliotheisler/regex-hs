
-- ExampleTree data, left branches, and right branches
data ExampleTree    = ExampleTree  String [ExampleTree] [ExampleTree]
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

