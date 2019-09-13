-- one optimization; all 9 nr digits can be skipped cause their total is 45 when pandigital; so dividable by 3
-- and the same is true for 8 digit nrs; cause the nr missing is 9, the total is 36, again dividable by 3

import Data.Char (digitToInt)
import Data.List

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

primes :: [Int]
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

isPandigital digits = let l = length digits in 
	                      null ([1..l] \\ digits)	

main = print . head $ filter isPrime $ filter (isPandigital . toDigits) [7654321,7654320..2]	                      