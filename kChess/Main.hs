module KChess where

import Control.Monad
import Data.Foldable
import Control.Monad.State.Lazy

data UnitKind = Archer | Horse | Pickmen deriving Show
data Position = Position { posX :: Int, posY :: Int} deriving (Show,Eq)
data Unit = Unit { uKind :: UnitKind, uPos :: Position } deriving Show
data Direction = North | South | East | West
type Error = Either String

boardSize :: Int
boardSize = 8

addUnit :: (Foldable m, MonadPlus m) => m Unit -> Unit -> Error (m Unit)
addUnit ob u = 
	if repeated
		then Left "The cell is not free."
		else Right (return u `mplus` ob)
	where
	repeated = Data.Foldable.any ((== uPos u) . uPos) ob

displace :: Direction -> Position -> Error Position
displace North (Position x y)
	| y>0 = Right $ Position x (y-1)
	| otherwise = Left "Unit out of board"
displace South (Position x y)
	| y<boardSize = Right $ Position x (y+1)
	| otherwise = Left "Unit out of board"
displace East (Position x y)
	| x>0 = Right $ Position (x-1) y
	| otherwise = Left "Unit out of board"
displace West (Position x y)
	| x<boardSize = Right $ Position (x+1) y
	| otherwise = Left "Unit out of board"

unit :: Unit
unit = Unit Archer $ Position 3 4

main :: IO ()
main = do
	let eb = addUnit [] unit >>= \x -> return (showBoard x)
	case eb of
		Right s -> putStrLn s
		Left e -> putStrLn e

-- Helpers

-- Puts charcter c at position p in string s
strPutChar :: String -> Char -> Int -> Error String
strPutChar s c p 
	| p > l = Left "Position is out of string."
	| otherwise = Right $ h ++ [c] ++ tail t
	where
	l = length s
	(h,t) = splitAt p s

unitKindChar :: UnitKind -> Char
unitKindChar Archer = 'A'
unitKindChar Pickmen = 'P'
unitKindChar Horse = 'H'

showBoard :: (Foldable m) => m Unit -> String
showBoard units = unlines $ execState fill b-- $ snd $ runState fill b
	where
	b = replicate boardSize $ replicate boardSize '.'
	fill = Data.Foldable.foldrM ff b units
	ff u ob = do
		let x = posX $ uPos u
		let y = posY $ uPos u
		let c = unitKindChar $ uKind u
		let ns = case strPutChar (ob!!y) c x of
						Left _ -> ob!!y
						Right rs -> rs
		put $ take y ob ++ [ns] ++ drop (y+1) ob
		get

