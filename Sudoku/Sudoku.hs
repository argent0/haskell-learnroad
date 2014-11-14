module Sudoku where

import Control.Applicative

--A move (x,y,n) means put n in the square at (x,y)

base :: Int
base = 3

size :: Int
size = base*base

data Move = Move Int Int Int deriving Show

allMoves :: [Move]
allMoves = Move <$> [1..size] <*> [1..size] <*> [1..size]

--Removes Move from [Move] and all thus prohibited moves
makeMove :: Move -> [Move]  -> [Move]
makeMove m = enforceRowRule m . enforceColRule m . enforceAreaRule m . enforceOneValuePerCell m

makeMoves :: [Move] -> [Move]
makeMoves = foldr makeMove allMoves

enforceRowRule :: Move -> [Move]  -> [Move]
enforceRowRule _ [] = []
enforceRowRule cm@(Move cx _ cv) (rm@(Move x _ v):ms) =
	if (x==cx) && (v==cv)
		then enforceRowRule cm ms
		else rm : enforceRowRule cm ms

enforceColRule :: Move -> [Move]  -> [Move]
enforceColRule _ [] = []
enforceColRule cm@(Move _ cy cv) (rm@(Move _ y v):ms) =
	if (y==cy) && (v==cv)
		then enforceColRule cm ms
		else rm : enforceColRule cm ms

enforceAreaRule :: Move -> [Move] -> [Move]
enforceAreaRule _ [] = []
enforceAreaRule cm@(Move cx cy cv) (rm@(Move x y v):ms) =
	if div (x-1) base == div (cx-1) base && div (y-1) base == div (cy-1) base && cv==v
		then enforceAreaRule cm ms
		else rm : enforceAreaRule cm ms

enforceOneValuePerCell :: Move -> [Move] -> [Move]
enforceOneValuePerCell _ [] = []
enforceOneValuePerCell cm@(Move cx cy _) (rm@(Move x y _):ms) =
	if (x==cx) && (y==cy)
		then enforceOneValuePerCell cm ms
		else rm : enforceOneValuePerCell cm ms
