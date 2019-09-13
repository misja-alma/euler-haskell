module Euler130 where

import Data.List
import Data.Maybe

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

divides :: Int -> Int -> Bool
divides x y = x `rem` y == 0

isPrime :: Int -> Bool
isPrime x
  | x < 2     = False
  | otherwise = all (not . divides x) $ takeWhile (\d -> d * d <= x) primes
  
-- Note: since mod. powers are cyclic, this could be speed up for large values of x.
modPowers :: Int -> [Int]
modPowers x = modPowers' 1
              where a = 10 `rem` x
                    modPowers' s = s : modPowers' (a * s `rem` x) 

firstRepUnit :: Int -> Int
firstRepUnit = (+1) . fromJust . elemIndex 0 . powerSums
               where powerSums x = scanl1 (\ss s -> (ss + s) `rem` x) (modPowers x)

solutions :: [Int]
solutions = filter (not . isPrime) $ filter (\x -> (x - 1) `rem` firstRepUnit x == 0) [x | x <- [7, 9 ..], x `rem` 5 /= 0]

main = print $ sum $ take 25 solutions