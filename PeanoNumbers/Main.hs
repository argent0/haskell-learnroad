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
