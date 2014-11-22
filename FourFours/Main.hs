module Main where

import Control.Applicative

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
