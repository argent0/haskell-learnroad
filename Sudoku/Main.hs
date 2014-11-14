module Main where

import Parse
import Sudoku
import Debug.Trace
import Control.Applicative
import Data.List
import Control.Monad

sortByOptions :: [Move] -> [Move]
sortByOptions = join . map snd . sortBy groupComp . groupCount . sortBy comp
	where
	comp (Move a b _) (Move c d _) = compare (a,b) (c,d)
	groupCount [] = []
	groupCount (m:ms) = (length l + 1, m:l) : groupCount (dropWhile eqm ms)
		where
		l = takeWhile eqm ms
		eqm x = comp m x == EQ
	groupComp (a,_) (b,_) = compare a b

solve :: [Move] -> [Move]
solve moves = snd (go moves (size*size-length moves) remainingMoves)
	where
	go mvs 0 [] = (True, mvs)
	--go missing [] = trace (show missing ++ "\n") (False, [])
	go mvs _ [] = (False, mvs)
--	go missing (m:ms) = --trace (show (m:ms) ++ "\n") $
--		m : go (missing-1) (makeMove m ms)
	go mvs missing (m:ms) = trace (show mvs ++ show ",FLUSH,") $
		if solvable && s 
			then (True, next)
			else go mvs missing (sortByOptions ms)
		where
		(s,next) = go (m:mvs) (missing-1) (sortByOptions (makeMove m ms))
		--solvable = foldr (\(x,y) b -> hasOptions (x,y) && b) True allPositions
		solvable = all hasOptions allPositions
		hasOptions (a,b) = or $ (\(Move c d _) -> (a==c) && (b==d)) <$> (m:mvs ++ ms)
		allPositions = (,) <$> [1..size] <*> [1..size]
	remainingMoves = sortByOptions $ makeMoves moves

main :: IO ()
main =
	getContents >>= \c ->
	( \p -> case p of
		Left e -> print e
		Right s -> print $ solve s) (parseSudoku c)
