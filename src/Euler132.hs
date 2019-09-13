module Euler132 ( main ) where

import Data.List
import Data.Maybe

-- fast way to determine if (1)(10^9) divides by x.
-- use sum of modPowers and use the fact that the powers are cyclic modulo x:
-- determine mod powers in 1 cycle starting at a. Note: when starting at 1, the cycle starts sometimes only at a, that's why we choose a. Check: is a always part of the cycle?
-- calc. the nr of cycle-lengths that fit in 10^9 (-1?)
-- modSum = nr * cycleSum + the remaining mod powers to complete length 10^9

-- if modSum rem x == 0 then x divides (1)(10^9)

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

divides :: Int -> Int -> Bool
divides x y = x `rem` y == 0

isPrime :: Int -> Bool
isPrime x
  | x < 2     = False
  | otherwise = all (not . divides x) $ takeWhile (\d -> d * d <= x) primes

mod10Powers :: Int -> [Int]
mod10Powers x = let a = 10 `rem` x in iterate (\s -> s * a `rem` x) a

cycleLength :: [Int] -> Int
cycleLength xs = 1 + fromJust (elemIndex (head xs) (tail xs))

-- Requires that x < 1(n)
powerSum :: Int -> Int -> Integer
powerSum n x = let powers = mod10Powers x 
                   cLength = cycleLength powers 
                   cycleSum = sum $ toInteger <$> take cLength powers in
               cycleSum * toInteger ((n - 1) `div` cLength) + toInteger (sum (take ((n - 1) `rem` cLength) powers)) + 1 
               
repUnitDivisibleBy n x = powerSum n x `rem` toInteger x == 0       

primeRepUnitDividers n = filter (repUnitDivisibleBy n) primes    

main = print $ sum $ take 40 (primeRepUnitDividers 1000000000)    