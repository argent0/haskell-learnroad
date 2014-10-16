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

{- Writing peano numbers in haskell -}
module Main where

{-
- exio4 > argent0: what is wrong with data Nat = Z | S Nat?
- exio4 > argent0: plus (S a) b = S (plus a b) ; plus Z b = b ?
- http://www.haskell.org/haskellwiki/Peano_numbers
-}

data Nat = Zero | Succ Nat

instance Show Nat where
	show Zero = "0"
	show (Succ n) = "S(" ++ show(n)

plus :: Nat -> Nat -> Nat
plus Zero b = b
plus (Succ a) b = Succ(plus a b)

ord :: Int -> Nat
ord 0 = Zero
ord n = Succ (ord (n - 1))

main :: IO ()
main = putStrLn "Hello, World!"

-- vim: expandtab
