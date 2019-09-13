module Euler129 where

import Data.List
import Data.Maybe

-- Note: since mod. powers are cyclic, this could be speed up for large values of x.
modPowers :: Int -> [Int]
modPowers x = modPowers' 1
              where a = 10 `rem` x
                    modPowers' s = s : modPowers' (a * s `rem` x) 

firstRepUnit :: Int -> Int
firstRepUnit = (+1) . fromJust . elemIndex 0 . powerSums
               where powerSums x = scanl1 (\ss s -> (ss + s) `rem` x) (modPowers x)

solveFor :: Int -> Int
solveFor n = let firstOdd = if even n then n + 1 else n in
             fromJust $ find (\x -> firstRepUnit x > n) [x | x <- [firstOdd, firstOdd + 2 ..], x `rem` 5 /= 0]

main = print $ solveFor 1000000
  
             
            