-- Start at 1000, end at 999x
-- We want a prime of 4 digits that has 2 permutations, which are different, and are also prime;
-- On top of that they should have the same distances. As an optimization we add that the 2 permutations have to be bigger.

import Data.List

primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

isPrime' x (p : prms) 
			| p * p > x                  = True
			| (x `mod` p) == 0           = False  
			| otherwise                  = isPrime' x prms

isPrime :: Integer -> Bool
isPrime x 
	| x < 2     = False
	| otherwise = isPrime' x primes

-- strangely enough the program seemed much slower after adding a 'nub' to remove double permutations
primePermutations p = let ps = filter isPrime $ map read $ nub $ permutations (show p) in 
					      if all (>=p) ps then ps else [] 	

areFirst3EntriesMetric ps = (ps !! 1) - head ps == (ps !! 2) - (ps !! 1)

-- Find the longest subset of xs where members have the same distance. Or, just a subset of length 3.
numbersWithSameDistance xs = if length xs < 3 then take 2 xs 
	                                          else let results = filter areFirst3EntriesMetric $ permutations xs in
	                                                   if null results then take 2 xs
	                                                   	               else take 3 $ head results

toPermsWithSameDistance p = let perms = primePermutations p in
                                numbersWithSameDistance perms

main = print $ map (concatMap show . sort) $ filter (\psd -> length psd == 3) $ map toPermsWithSameDistance $ takeWhile (<= 9999) $ dropWhile (< 1000) primes
