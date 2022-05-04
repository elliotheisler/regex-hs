import Text.Parsec
import Text.Parsec.String
import Data.List (intersperse)


type LowerBound = Int
data UpperBound = Unlimited | Upper Int
-- precedence decreases downward
data RETree = 
    Symbol Char
  | Repetition RETree LowerBound UpperBound
  | Concat [RETree]
  | Union [RETree]

instance Show RETree where
    show reTree = "/" ++ pShow 4 reTree ++ "/"

brackets :: Int -> Int -> String -> String
brackets i j s
  | i <= j = "("++s++")"
  | otherwise = s

{- precedence-Show. convert this regex tree to is string form, making sure
 - to place it in brackets if its parent has lower-or-equal precedence -}
pShow :: Int -> RETree -> String
pShow i (Symbol c) = [c]

pShow i (Repetition re lower Unlimited)
  | lower == 1 = brkts $ ps re ++ "+"
  | lower == 0 = brkts $ ps re ++ "*"
  | otherwise  = brkts $ ps re ++ "{" ++ show lower ++ ",}"
  where brkts = brackets i 1
        ps = pShow 1
pShow i (Repetition re lower (Upper upper))
  | lower == upper = brkts $ ps re ++ "{" ++ show lower ++ "}"
  | otherwise = brkts $ ps re ++ "{" ++ show lower ++ "," ++ show upper ++ "}"
  where brkts = brackets i 1
        ps = pShow 1

pShow i (Concat rs) = brackets i 2 . concat . map (pShow 2) $ rs

pShow i (Union rs) = brackets i 3 . concat . intersperse "|" . map (pShow 3) $ rs

type REParser = Parsec String () RETree

parseRegex :: REParser
parseRegex = between (char '/') parseUnion (char '/')

parsePrimary :: REParser
parsePrimary = choice $ try <$> 
    [(between (char '(') (char ')') parseUnion),
     Symbol <$> (char '\\' >> anyChar),
     Symbol <$> noneOf ")|"] -- i.e. dont go past this bracket group or union element

parseRepetition :: REParser
parseRepetition = do
    prim <- parsePrimary
    range <- optionMaybe . try $ parseRange
    case range of Just (lower, upper) -> return (Repetition prim lower upper)
                  Nothing             -> return prim

parseRange :: Parsec String () (LowerBound, UpperBound)
parseRange = do
    char '{'
    lower <- parseInt
    n <- try $ char ',' <|> char '}'
    if n == '}'
        then return (lower, Upper lower)
        else do
            upper <- optionMaybe . try $ parseInt
            char '}'
            case upper of Nothing -> return (lower, Unlimited)
                          Just u  -> return (lower, Upper u  )
    where parseInt = (read :: String -> LowerBound) <$> many1 digit

parseConcat :: REParser
parseConcat = do
    repList <- many parseRepetition
    case repList of [r]  -> return r
                    list -> return (Concat list)
                       
parseUnion :: REParser
parseUnion = do
    unionList <- sepBy parseConcat (char '|')
    case unionList of [r]  -> return r
                      list -> return (Union list)
