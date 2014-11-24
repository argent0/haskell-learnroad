module Main where

import Control.Applicative
import Data.Maybe
import Data.List

data Expr =
	Empty
	| Value
	| TwoValue
	| ThreeValue
	| FourValue
	| Neg Expr
	| Fac Expr
	| Sqr Expr
	| Plus Expr Expr
	| Mul Expr Expr
	| Div Expr Expr
	| Mod Expr Expr

instance Show Expr where
	show Empty = ""
	show Value = "4"
	show TwoValue = "44"
	show ThreeValue = "444"
	show FourValue = "4444"
	show (Neg e) = "-(" ++ show e ++ ")"
	show (Fac Value) = "4!"
	show (Fac e) = "(" ++ show e ++ ")!"

	show (Sqr Value) = "√4"
	show (Sqr e) = "√(" ++ show e ++ ")"

	show (Plus Value Value) = "4+4"
	show (Plus Value TwoValue) = "4+44"
	show (Plus Value ee) = "4+(" ++ show ee ++ ")"
	show (Plus e Value) = "(" ++ show e ++ ")+4"
	show (Plus e TwoValue) = "(" ++ show e ++ ")+44"
	show (Plus e ee) = "(" ++ show e ++ ")+(" ++ show ee ++ ")"

	show (Mul Value Value) = "4*4"
	show (Mul Value TwoValue) = "4*44"
	show (Mul Value ee) = "4*(" ++ show ee ++ ")"
	show (Mul TwoValue ee) = "44*(" ++ show ee ++ ")"
	show (Mul ThreeValue ee) = "444*(" ++ show ee ++ ")"
	show (Mul e Value) = "(" ++ show e ++ ")*4"
	show (Mul e TwoValue) = "(" ++ show e ++ ")*44"
	show (Mul e ThreeValue) = "(" ++ show e ++ ")*444"
	show (Mul e ee) = "(" ++ show e ++ ")*(" ++ show ee ++ ")"

	show (Div Value Value) = "4/4"
	show (Div Value ee) = "4/(" ++ show ee ++ ")"
	show (Div TwoValue ee) = "44/(" ++ show ee ++ ")"
	show (Div ThreeValue ee) = "444/(" ++ show ee ++ ")"
	show (Div e Value) = "(" ++ show e ++ ")/4"
	show (Div e TwoValue) = "(" ++ show e ++ ")/44"
	show (Div e ThreeValue) = "(" ++ show e ++ ")/444"
	show (Div e ee) = "(" ++ show e ++ ")/(" ++ show ee ++ ")"

	show (Mod Value Value) = "4%4"
	show (Mod Value ee) = "4%(" ++ show ee ++ ")"
	show (Mod TwoValue ee) = "44%(" ++ show ee ++ ")"
	show (Mod ThreeValue ee) = "444%(" ++ show ee ++ ")"
	show (Mod e Value) = "(" ++ show e ++ ")%4"
	show (Mod e TwoValue) = "(" ++ show e ++ ")%44"
	show (Mod e ThreeValue) = "(" ++ show e ++ ")%444"
	show (Mod e ee) = "(" ++ show e ++ ")%(" ++ show ee ++ ")"

factorial :: Integer -> Maybe Integer
factorial 0 = Just 1
factorial n
	| n>0 && n<10 = (n*) <$> factorial (n-1)
	| otherwise = Nothing

secSqrt :: Integer -> Maybe Integer
secSqrt n
	| (n<0) = Nothing
	| (n==4) = Just 2
	| otherwise = if (r*r) == n then Just r else Nothing
	where r = floor $ sqrt $ fromInteger n

secDiv :: Integer -> Integer -> Maybe Integer
secDiv _ 0 = Nothing
secDiv a b = Just $ div a b

secMod :: Integer -> Integer -> Maybe Integer
secMod _ 0 = Nothing
secMod a b = Just $ mod a b

eval :: Expr -> Maybe Integer
eval Empty = Just 0
eval Value = Just 4
eval TwoValue = Just 44
eval ThreeValue = Just 444
eval FourValue = Just 4444
eval (Neg e) = (0-) <$> eval e
eval (Fac e) = eval e >>= factorial
eval (Sqr e) = eval e >>= secSqrt
eval (Plus e ee) = (+) <$> eval e <*> eval ee
eval (Mul e ee) = (*) <$> eval e <*> eval ee
eval (Div e ee) = eval e >>= \a -> eval ee >>= \b -> secDiv a b
eval (Mod e ee) = eval e >>= \a -> eval ee >>= \b -> secMod a b
eval e = error $ "Unrecognized expression: " ++ show e

-- Populate: values left, root expr, 
populate :: Bool -> Bool -> Bool -> Int -> [Expr]
populate _ _ _ 0 = [Empty]
populate _ _ _ 1 = [Value, Fac Value, Sqr Value]
populate sqr fac neg n =
	(if n==4 then [FourValue] else []) ++
	(if n==3 then [ThreeValue] else []) ++
	(if n==2 then [TwoValue] else []) ++
	concatMap (\d -> Plus <$> npopulate (n-d) <*> npopulate d) [1..(n-1)] ++
	concatMap (\d -> Mul <$> npopulate (n-d) <*> npopulate d) [1..(n-1)] ++
	concatMap (\d -> Div <$> npopulate (n-d) <*> npopulate d) [1..(n-1)] ++
	concatMap (\d -> Mod <$> npopulate (n-d) <*> npopulate d) [1..(n-1)] ++
	if neg then [] else (Neg <$> populate False False True n) ++
	if fac then [] else (Fac <$> populate False True False n) ++
	if sqr then [] else (Sqr <$> populate True False False n)
	where npopulate = populate False False False

main :: IO ()
main = putStr $ unlines $ map (\(r,e) -> show r ++ "\t=" ++ show e ) $
	sortBy (\(r,_) (rr,_) -> compare r rr) $
	catMaybes $ map (\x -> if isJust (eval x) then Just (fromJust (eval x), x) else Nothing) $ populate False False False 4

{-
go :: Int -> Int -> Int -> Char -> [String]
go len paren n prev
	| n<=3 = case prev of
		'4' -> 
			((++) <$> ["4"] <*> ngo paren (n+1) '4') ++
			((++) <$> ["+"] <*> ngo paren n '+') ++
			((++) <$> ["*"] <*> ngo paren n '*') ++
			((++) <$> ["/"] <*> ngo paren n '/') ++
			((++) <$> ["-"] <*> ngo paren n '-') ++
			((++) <$> ["^"] <*> ngo paren n '^') ++
			if (paren>0)
				then ((++) <$> [")"] <*> ngo (paren-1) n ')')
				else []
		')' -> 
			--((++) <$> ["4"] <*> ngo paren (n+1) '4') ++
			((++) <$> ["+"] <*> ngo paren n '+') ++
			((++) <$> ["*"] <*> ngo paren n '*') ++
			((++) <$> ["/"] <*> ngo paren n '/') ++
			((++) <$> ["-"] <*> ngo paren n '-') ++
			((++) <$> ["%"] <*> ngo paren n '%') ++
			if (paren>0)
				then ((++) <$> [")"] <*> ngo (paren-1) n ')')
				else []
		'+' ->
			((++) <$> ["4"] <*> ngo paren (n+1) '4') ++
			((++) <$> ["("] <*> ngo paren n '(')
		'*' ->
			((++) <$> ["4"] <*> ngo paren (n+1) '4') ++
			((++) <$> ["("] <*> ngo paren n '(')
		'/' ->
			((++) <$> ["4"] <*> ngo paren (n+1) '4') ++
			((++) <$> ["("] <*> ngo paren n '(')
		'%' ->
			((++) <$> ["4"] <*> ngo paren (n+1) '4') ++
			((++) <$> ["("] <*> ngo paren n '(')
		'-' ->
			((++) <$> ["4"] <*> ngo paren (n+1) '4') ++
			((++) <$> ["("] <*> ngo paren n '(')
		' ' -> (++) <$> ["4"] <*> ngo paren (n+1) '4'
		'(' -> (++) <$> ["4"] <*> ngo (paren+1) (n+1) '4'
		'^' -> (++) <$> ["("] <*> ngo paren n '('
	| otherwise = [replicate paren ')']
	where ngo = go (len+1)

ops :: [String]
ops = go 0 0 0 ' '

main :: IO ()
main = 
	putStrLn "use v5.20; my $i;" >>= \_ ->
	putStrLn $ unlines $ map (\x -> "$i=undef; eval { $i="++x++"};say \"$i\\t="++x++"\" if defined $i;") ops
-}
