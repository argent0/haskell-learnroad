{-
Copyright (C) 2014  Aner Luero

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

import Data.Foldable

data Tree k v = Empty | Leaf k v | Branch k v (Tree k v) (Tree k v) deriving Show

instance Foldable (Tree k) where
	foldr _ z Empty = z
	foldr f z (Leaf _ v) = f v z
	foldr f z (Branch _ v l r) = Data.Foldable.foldr
		f (f v (Data.Foldable.foldr f z r)) l

insert :: Ord k => Tree k v -> k -> v -> Tree k v
insert Empty k v= Leaf k v
insert (Leaf ok ov) k v
   | k < ok = Branch ok ov (Leaf k v) Empty
   | otherwise = Branch ok ov Empty (Leaf k v)
insert (Branch ok ov l r) k v
   | k < ok = Branch ok ov (insert l k v) r
   | otherwise = Branch ok ov l (insert r k v)

toTree :: (Foldable t, Ord k) => t k -> Tree k k
toTree = Data.Foldable.foldr (\kk b -> insert b kk kk) Empty

insertAndCountInversions :: Ord k => Integer -> k -> Tree k (Integer, Integer) -> (Tree k (Integer, Integer), Integer)
insertAndCountInversions acc k Empty = (Leaf k (0,0), acc)

insertAndCountInversions acc k (Leaf ok (vl,vr))
	| k < ok = (Branch ok (vl+1,vr) (Leaf k (0,0)) Empty, acc + 1)
	| otherwise = (Branch ok (vl,vr+1) Empty (Leaf k (0,0)), acc)

insertAndCountInversions acc n (Branch ok (vl,vr) l r)
	| n < ok = (Branch ok (vl+1,vr) new_l_branch r, cl)
	| otherwise = (Branch ok (vl,vr+1) l new_r_branch, cr)
	where 
		(new_l_branch,cl) = insertAndCountInversions (acc+vr+1) n l
		(new_r_branch,cr) = insertAndCountInversions acc n r


insertAndCountInversions' :: Ord k => k -> Tree k (Integer, Integer) -> (Tree k(Integer, Integer), Integer)

insertAndCountInversions' k Empty = (Leaf k (0,0), 0)
insertAndCountInversions' k (Leaf ok (vl, vr))
	| k < ok = (Branch ok (vl+1,vr) (Leaf k (0,0)) Empty, 1)
	| otherwise = (Branch ok (vl,vr+1) Empty (Leaf k (0,0)), 0)
insertAndCountInversions' k (Branch ok (vl, vr) l r)
	| k < ok = (Branch ok (vl+1,vr) new_l_branch r, cl+vr+1)
	| otherwise = (Branch ok (vl,vr+1) l new_r_branch, cr)
	where
		(new_l_branch,cl) = insertAndCountInversions' k l
		(new_r_branch,cr) = insertAndCountInversions' k r

countInversions' :: (Foldable c, Ord k) => c k -> (Tree k (Integer, Integer), Integer)
countInversions' = Data.Foldable.foldl f (Empty,0)
	where
	f (ot,op) k = (nt, op+np)
		where (nt, np) = insertAndCountInversions' k ot

countInversions :: (Foldable c, Ord k) => c k -> (Tree k (Integer,Integer), Integer)
countInversions = Data.Foldable.foldl f (Empty,0)
	where
	f acc n = (nt, np)
		where
		(nt,np) = insertAndCountInversions p n t
		(t,p) = acc


main :: IO ()
main = 
	getContents >>= print . snd . countInversions . fmap r . lines
	where r cc = read cc :: Integer
