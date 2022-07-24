{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.Cont
import Data.Monoid
import Data.List (sortOn)

type Row = Int
type Col = Row
type Point = (Row, Col)
data Board = Board [Point] [Point]
newtype SortedRepr a = SortedRepr { unSorted :: [a] }

instance Show Board where
    show b = "Board:" <> show ( toSorted b )

toSorted :: Board -> SortedRepr Col
toSorted (Board queens _) = 
    SortedRepr $ (\(_,c) -> c) <$> sortOn (\(r,c) -> r) queens 

instance Show (SortedRepr Col) where
    show (unSorted -> []) = ""
    show (unSorted -> c:tail) = 
      "\n" <>
      replicate c ' ' <>
      "Q" <> 
      replicate (7-c) ' ' <> 
      show ( SortedRepr tail :: SortedRepr Col )

backtrack :: Board -> Row -> Col -> [Board]
backtrack _ i j
  | i < 0 || i >  8 = undefined
  | j < 0 || j >= 8 = undefined
backtrack _ 8 _ = mempty
backtrack b@(Board queens occupied) i 7
  | (i, 7) `elem` occupied = backtrack b (i+1) 0
  | otherwise = backtrack (plotQueen b i 7) (i+1) 0
backtrack b@(Board queens occupied) i j
  | (i, j) `elem` occupied = nextInRow
  | otherwise = backtrack (plotQueen b i j) i (j+1) <> nextInRow
  where
    nextInRow = backtrack b i (j+1)

plotQueen :: Board -> Row -> Col -> Board
plotQueen b@(Board queens occupied) row col = 
    Board ( (row,col):queens ) ( nextOccupied <> occupied )
  where
    nextOccupied = column <> diagL <> diagR
    column = [ (r,col) | r <- [row+1..7] ]
    diagL  = [ rowCol  | rowCol <- [row+1..7] `zip` reverse [0..col-1] ]
    diagR  = [ rowCol  | rowCol <- [row+1..7] `zip` [col+1..7] ]

initBoard = Board [] []

main :: IO ()
main = do
  let q = plotQueen initBoard 2 1
  print q     
  let btrack = backtrack initBoard 0 0
  print btrack

