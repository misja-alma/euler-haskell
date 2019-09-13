-- For each number n ; find a prime p < n where div (n - p) 2, is possible, and is a square. 

-- Would be handy to have a fast int sqrt function.
-- What about a lookup table? TODO check the performance of a simple list vs a Set; would the latter use hashes or so?

-- TODO the building of the set takes too much time.
--primesSet = Set.fromAscList (takeWhile (<10000) primes)

--isPrimeLookup n = n `Set.member` primesSet 
--import TimeIt
import Data.List

primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

isPrime' x (p : prms) 
			| p * p > x                  = True
			| (x `mod` p) == 0           = False  
			| otherwise                  = isPrime' x prms

isPrime x 
	| x < 2     = False
	| otherwise = isPrime' x primes


doubleSquares = map ((*2) . (^2)) [0..]

isDoubleSquare' x (s  : sqrs)
             | x == s    = True
             | x < s     = False
             | otherwise = isDoubleSquare' x sqrs

isDoubleSquare x = isDoubleSquare' x doubleSquares

oddCompositeNumbers = filter (not . isPrime) [3,5..]

isPrimePlusDoubleSquare n = any (\p -> isDoubleSquare(n - p)) (takeWhile (<n) primes)

nonGoldbachs = filter (not . isPrimePlusDoubleSquare) oddCompositeNumbers

main = print $ head nonGoldbachs
