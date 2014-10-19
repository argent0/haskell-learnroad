{-
Copyright (C) 2014 Aner Lucero

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

module Main where

import System.Random
import Data.List

circuits :: Eq a => [a] -> [[a]]
circuits [] = []
circuits [x] = [[x]]
circuits (x:xs) = concatMap (\c -> map (c:) $ circuits (coset c (x:xs))) (x:xs)
	where coset l = filter (/=l)

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x,y) (xx,yy) = sqrt $ ((x-xx)**2) + ((y-yy)**2)

travelLenght :: [Point] -> (Double, [Double], [Point])
travelLenght [] = (0,[],[])
travelLenght (x:xs) = (sum distances, distances, x : xs)
	where
	distances = zipWith distance (x:xs) (xs ++ [x])


nRandomPoints :: Int -> IO [Point]
nRandomPoints 0 = return []
nRandomPoints n = do
	a <- r
	b <- r
	m <- nRandomPoints (n-1)
	return $ (a,b) : m
	where r = randomIO :: IO Double

normalizePoints :: [Point] -> [Point]
normalizePoints ps =
	map normalize ps
	where
	normalize (x,y) = (x/maxx, y/maxy)
	maxx = maximum $ map fst ps
	maxy = maximum $ map snd ps

main :: IO ()
main = 
	nRandomPoints 4 >>= \points ->
	print $ minimumBy (\(x,_,_) (xx,_,_) -> compare x xx) $
		map travelLenght $ circuits $ normalizePoints points
