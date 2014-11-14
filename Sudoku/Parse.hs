module Parse where

import Text.ParserCombinators.Parsec
import Control.Monad
import Sudoku

{- A sudoku fle contains 9 lines of . or numbers 0-9 -}

sudokuFile :: GenParser Char st [Move]
sudokuFile = do
	ls <- forM [1..size] (\y -> do
		l <- sudokuLine y
		_ <- eol
		return l
		)
	return $ concat ls


---- Each line contains 1 or more cells, separed by a comma
sudokuLine :: Int -> GenParser Char st [Move]
sudokuLine y = do
	content <- forM [1..size-1] (\x -> do
		c <- cell y x
		_ <- char ' '
		case c of
			Nothing -> return Nothing
			Just m -> return $ Just m
		)
	f  <- cell y size

	return $ map (\(Just x) -> x) $ filter (\mm ->
		case mm of
			Nothing -> False
			Just _ -> True
			) (content ++ [f])

cell :: Int -> Int -> GenParser Char st (Maybe Move)
cell y x =
	oneOf "123456789." >>= \c ->
	case c of
		'.' -> return Nothing
		cc -> return $ Just (Move x y (read [cc] :: Int))

eol :: GenParser Char st Char
eol = char '\n'
--
parseSudoku :: String -> Either ParseError [Move]
parseSudoku = parse sudokuFile "(unknown)"
