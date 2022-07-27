{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad.Cont
import Data.Monoid
import Data.List (sortOn)

type Row = Int
type Col = Row
type Point = (Row, Col)
data Board = Board [Point] [Point]

instance Show Board where
    show (Board queens _) = "Board: " <> showSolution queens
    
{- showSolution: only correctly prints for a complete solution where each row has exactly 1 queen -}
showSolution :: [Point] -> String
showSolution [] = mempty
showSolution ( (row,col):queens ) =
      "\n" <>
      mconcat ( replicate col "_ " ) <>
      "Q " <> 
      mconcat ( replicate (7-col) "_ " ) <> 
      showSolution queens

backtrack :: Board -> Row -> Col -> [Board]
backtrack _ i j
  | i < 0 || i > 8 = undefined
  | j < 0 || j > 8 = undefined
backtrack b 8 _ = return b
backtrack b@(Board queens _) i 8 = case queens of
    [] -> mempty
    (row,col):queens | row == i  -> backtrack b (i+1) 0
                     | otherwise -> mempty -- already tried all pieces in this row
backtrack b@(Board queens occupied) i j
  | (i, j) `elem` occupied = nextInRow
  | otherwise = backtrack (plotQueen b i j) (i+1) 0 <> nextInRow
  where
    nextInRow = backtrack b i (j+1)

backtrackC :: Board -> Row -> Col -> Cont r Board
backtrackC _ i j
  | i < 0 || i > 8 = undefined
  | j < 0 || j > 8 = undefined
backtrackC b 8 _ = return b
backtrackC b@(Board queens _) i 8 = case queens of
    [] -> mempty 
    (row,col):queens | row == i  -> backtrackC b (i+1) 0
                     | otherwise -> mempty -- already tried all pieces in this row
backtrackC b@(Board queens occupied) i j
  | (i, j) `elem` occupied = nextInRow
  | otherwise = callCC $ k $ backtrackC (plotQueen b i j) (i+1) 0 nextInRow
  where
    nextInRow = backtrackC b i (j+1)


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

