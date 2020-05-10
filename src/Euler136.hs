-- n's with 1 solution:
-- [3,4,7,11,12,16,19,20,23,28,31,43,44,47,48,52,59,67,68,71,76,79,80,83,92]

-- solutions are of one of these forms:
-- 0. prime = 4p - 1 for p >= 1
-- 1. 4a * 4b  = 4 * 4p. To avoid duplicates, p has to be 1 or an odd prime
-- 2. (4a + 2) * (4b + 2). If we enumerate them, a can be 0 and b >= 1. It can be shown that b has to be 2 * (1 or an odd prime)

-- algo: generate all primes until 50 million. Count solutions for 0, count solutions for 1. and 2. that are not too big.
-- note that solutions for 1. and 2. do not have to be generated because they are monotonously increasing, we just have to search
-- for the first solution that exceeds 50 million and count the length of the series until there. 
-- If we have the primes with their indices cached, e.g. from step 1, then this could be done with binary search.
module Euler136 where 

import Data.Numbers.Primes

divides4 x = x `rem` 4 == 0 

primeSolutions :: Int -> [Int]
primeSolutions m = filter (\p -> divides4 (p + 1)) $ takeWhile (< m) primes

square4Solutions :: Int -> [Int]
square4Solutions m = takeWhile (< m) $ fmap (* 16) (1 : tail primes)

a2b2Solutions :: Int -> [Int]
a2b2Solutions m = takeWhile (< m) $ fmap (\p -> 4 * p) (1 : tail primes)

solve :: Int -> Int
solve m = length (primeSolutions m) + length (square4Solutions m) + length (a2b2Solutions m)

main = print $ solve 50000000

