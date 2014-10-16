{-
 - REPL that plays Rock, Paper Scissors. It uses Markow chains to predict
 - player moves.
 -
 - As an improvement I could make it default to a random guess when the
 - certanity of the prediction is low.
 -}
module Main where

import Control.Monad
import Data.List (group, sort)
import Debug.Trace

data Weapon = Rock | Paper | Scissors deriving (Show, Eq)

instance Ord Weapon where
	compare Rock Paper = LT
	compare Rock Scissors = GT
	compare Paper Scissors = LT
	compare Paper Rock = GT
	compare Scissors Rock = LT
	compare Scissors Paper = GT
	compare Rock Rock = EQ
	compare Paper Paper = EQ
	compare Scissors Scissors = EQ

weapon :: [Char] -> Either String Weapon
weapon [x] = fromChar x
	where
	fromChar 'r' = Right Rock
	fromChar 'p' = Right Paper
	fromChar 's' = Right Scissors
	fromChar c = Left $ "Unknown weapon: " ++ [c]
weapon w = Left $ "Unknown weapon: " ++ w

weaponToBeat :: Weapon -> Weapon
weaponToBeat Rock = Paper
weaponToBeat Paper = Scissors
weaponToBeat Scissors = Rock

transitions :: Int -> [a] -> [[a]]
transitions _ [] = []
transitions 1 l = map (:[]) l
transitions n (x:xs) =
	if (length trans) < n then [] else [trans] ++ (transitions n xs)
	where trans = take n (x:xs)

--freqCounts :: Ord a => [[a]] -> [(a, Int)]
freqCounts l = map sampleAndCount $ group $ sort l
	where sampleAndCount x = (head x, length x)

marcovQuery :: Ord a => Int -> [a] -> [a] -> [([a],Int)]
marcovQuery depth history query =  freqCounts $ filter ((==query) .init) ts
	where ts = transitions (depth + 1) history

marcovianAdversary :: (Show a, Ord a) => Int -> [a] -> a
marcovianAdversary depth history = let (t,v) = the_max in 
   if null t then head history else last t
   where
   the_max = foldr _max ([],0) $ marcovQuery (depth) r_history query
   query = reverse $ take depth history
   r_history = reverse history
   _max (l,a) (ll,aa) = if a > aa then (l,a) else (ll,aa)

adversary :: [Weapon] -> Weapon
adversary [] = Rock
adversary (w:ws) = 
   if (length (take 20 (w:ws)) < 20)
      then weaponToBeat $ marcovianAdversary 2 (w:ws)
      else weaponToBeat $ marcovianAdversary 3 (w:ws)


showBattle :: Weapon -> Weapon -> Ordering -> String
showBattle wu wa br = show wu ++ " vs " ++ show wa ++ " ... " ++ result
	where result = case br of
		EQ -> "Draw :|"
		LT -> "You Lose :(" 
		GT -> "You Win :)" 

history_span :: Int
history_span = 100

repl :: [Weapon] -> Int -> Int -> IO ()
repl history wins lose = 
   putStrLn (show (adversary history)) >>
   putStrLn ("Score: " ++ show wins ++ "/" ++ show lose) >>
   putStr "Select your weapon:\n(r)ock, (p)aper, (s)issors.\n> " >>
   getLine >>= \user_input ->
   return (weapon user_input) >>= \ew -> --user weapon
   case ew of
      Left m -> putStrLn m >> repl history wins lose
      Right w ->
         return (adversary history) >>= \aw -> --adversary wepon
         return (compare w aw) >>= \br -> --battle result
         putStrLn (showBattle w aw br) >> case br of
            EQ -> repl (take history_span (w:history)) wins lose 
            GT -> repl (take history_span (w:history)) (wins + 1) lose 
            LT -> repl (take history_span (w:history)) wins (lose + 1)

main :: IO ()
main = 
	putStrLn "Welcome to Rock, Paper, Scissors!" >>
	repl [] 0 0

--	vim: expandtab
