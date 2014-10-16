{- Aritmetic addition couting the number of primitive operatios involved -}
modOp :: (Int -> Int -> Int) -> Int -> Int -> Int -> Int
modOp f base a b = mod (f a b) base

modSum = modOp (+) 10

primitiveAddition :: Int -> Int -> Int -> Int -> (Int, Int)
primitiveAddition base a b c = (digit, carry)
	where
		digit = foldr modSum 0 [a,b,c]
		carry =	if s >= base
						then div s base
						else 0
		s = (a + b + c)

type MyInteger = [Int]

myAdd :: Int -> MyInteger -> MyInteger -> MyInteger -> MyInteger
myAdd _ acc [] (y:ys) = acc ++ (y:ys)
myAdd _ acc (x:xs) [] = acc ++ (x:xs)
myAdd base acc [x] [y] = acc ++ [digit, carry]
	where
	(digit, carry) = primitiveAddition base x y 0
myAdd base acc (x:xs) (y:ys) =
	myAdd base (acc ++ [digit]) ([h + carry] ++ t) ys
	where
	(digit, carry) = primitiveAddition base x y 0
	t = if ((length . take 2) xs) == 2
				then tail xs
				else []
	h = if ((length . take 1) xs) == 1
				then head xs
				else 0

c_myAdd :: Int -> Int -> MyInteger -> MyInteger -> MyInteger -> (Int, MyInteger)
c_myAdd c _ acc [] (y:ys) = (c, acc ++ (y:ys))
c_myAdd c _ acc (x:xs) [] = (c, acc ++ (x:xs))
c_myAdd c base acc [x] [y] = (c + 1, acc ++ [digit, carry])
	where
	(digit, carry) = primitiveAddition base x y 0
c_myAdd c base acc (x:xs) (y:ys) =
	c_myAdd (c + 1) base (acc ++ [digit]) ([h + carry] ++ t) ys
	where
	(digit, carry) = primitiveAddition base x y 0
	t = if ((length . take 2) xs) == 2
				then tail xs
				else []
	h = if ((length . take 1) xs) == 1
				then head xs
				else 0
