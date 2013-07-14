import Data.List

minNum :: Int -> (Int, [Int]) -> Int
minNum a (n, xs)
	| n == 0 = a
	| m == (b - a) = minNum b (n - m, vs)
	| otherwise = minNum a (m, us)
	where
		(us, vs) = partition (< b) xs
		b = a + 1 + (n `div` 2)
		m = length us

main = 
	let xs = [0, 1, 2, 3] in
		print $ minNum 0 (length xs, xs)