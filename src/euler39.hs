import Data.List
import Data.Function (on)

-- a and b cant get too big, cause aa + bb == cc and a + b + c == p.
-- say that b = 1, then c = (p-a-1), then aa = (p-a-1)2 = p2 -2p(a+1) + (a+1)2 = p2 -2pa -2p + a2 +2a +1
-- 0 = p2 -2pa -2p + 0 -2a + 1
-- 0= p2 -a(2p + 1) -2p + 1
-- -p2 +2p -1 / (-2p -1) = a = (p-1)2/(2p+1)

-- b could be constrained as well once a is known, but havent done that (lazy).

trianglesForPerimeter :: Int -> [(Int, Int, Int)]
trianglesForPerimeter p = let maxPForAB = quot ((p-1) * (p-1))  (2 * p + 1) in 
	                          [ (a,b,c) | a <- [1..maxPForAB], b <- [1..p-a-1],  c <- [p - a - b], a*a + b*b == c*c]


main = print $ maximumBy (compare `on` length . trianglesForPerimeter) [3..1000]