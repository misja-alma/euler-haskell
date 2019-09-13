import Data.List
import Control.DeepSeq
import Test.QuickCheck

quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

-- Since primes is a top level definition and not a function, its values will be kept in memory during execution of the program. This speeds things up!

primes :: [Integer]
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

prop_isPrime :: Integer -> Bool
prop_isPrime x 
		| isPrime x  = all (\y -> x `mod` y  > 0) [2..(x-1)] 
		| otherwise  = any (\y -> x `mod` y  == 0) [2..(x-1)] 

polynomial a b x = x * x + a * x + b

primeRange a b x = if isPrime (polynomial a b x) then x : primeRange a b (x+1) else [] 
  
prop_primeRangeAllPrimes :: Integer -> Integer -> Bool
prop_primeRangeAllPrimes a b = all (isPrime . polynomial a b) $ primeRange a b 0

primeRangeLength a b = length $ primeRange a b 0

prTripleOrdering (prLength1, _, _) (prLength2, _, _) 
			| prLength1 > prLength2 = GT
			| prLength1 < prLength2 = LT
			| otherwise = EQ

paramsWithRangeLength = map (\(a,b) -> (primeRangeLength a b, a, b))

maxPrimeRange abTuples = maximumBy prTripleOrdering $ paramsWithRangeLength abTuples 


-- Some optimisations are possible; when n is 0, b is the only term left, so b should itself be a prime. 
-- Since for n =1, the equation becomes 1 + a + b, (1 + a) should not be odd when b is a prime. So skip all even a's.
-- Also, primes are > 1. so for n = 1, a + b > 1. This can be done nicely with a list comprehension .. 

generateAbTuples = let rangeA = [-999,-997..999];
                       rangeB = takeWhile (<1000) primes in
                 [(a, b) | a <- rangeA, b <- rangeB, a + b > 1]

main = print $ maxPrimeRange generateAbTuples 
