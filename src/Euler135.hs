module Euler135 where

import IntMath
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad (guard)
import qualified Data.Set as Set
import Data.List

intSqrtRounded :: Int -> Int
intSqrtRounded x = floor $ sqrt $ fromIntegral x

nrSolutions :: Int -> Int
nrSolutions n = let start = intSqrtRounded n `div` 2
                    end = (n + 1) `div` 4 + 1 in
                sum $ fmap solveForA' [start .. end]
                where solveForA' a = let d = 4 * a * a - n in
                                     if d < 0 then 0 else case intSqrt d of
                                                            Nothing -> 0
                                                            Just s  -> if s >= a || 2 * a - s == 2 * a + s then 1 else 2

solutions n = let start = intSqrtRounded n `div` 2
                  end = (n + 1) `div` 4 + 1 in
              [start .. end] >>= solveForA
              where solveForA a = let d = 4 * a * a - n in
                                  if d < 0 then [] else case intSqrt d of
                                                          Nothing -> []
                                                          Just s  -> let y = 2 * a + s in
                                                                     -- y has to be at least a + 1 so x can be at least 1
                                                                     -- Note: this can count solutions twice like for n = 4
                                                                     if s >= a || 2 * a - s == y then [(y, a)] else [(2 * a - s, a), (y, a)]

test n = let ss = solutions n in all (\(y, a) -> (y + a) * (y + a) - y * y - (y - a) * (y - a) == n) ss

solve = length $ filter (== 10) $ nrSolutions <$> [1 .. 1000000 - 1]

square :: Int -> Int
square x = x * x

isSquare :: Int -> Bool
isSquare x = isJust $ intSqrt x

-- returns the ceiling of x / 3, x should be positive
div3 :: Int -> Int
div3 x = if x `rem` 3 == 0 then x `div` 3 else x `div` 3 + 1

-- This could be sped up by only taking a subrange of a precomputed list of 4 square x.
-- Min and max would have to be adjusted with + n, the result would have to be mapped by -n.
-- The precomputed list could be stored in a Tree for fast subrange access
possibleYSquares :: Int -> [Int]
possibleYSquares n =  let maxYS = square $ (n - 1) `div` 2 in
                      takeWhile (<= maxYS) $ fmap (\a -> 4 * square a - n) [1..]

-- if possiblyYSquare (pbs) is a square, then pbs + n is a square as well since pbs = 4*x^2 - n.
-- for the same reason it is sure to divide 4
validYSquares :: Int -> [Int]
validYSquares n = filter isSquare (possibleYSquares n)

-- Note: this counts the nr of solutions, but there could still be 2 identical solutions ..  This is only the case when y = 0
countSolutions :: Int -> Int -> Int
countSolutions n ys = if ys /= 0 && ys < div3 n then 2 else 1

nrSolutions3 :: Int -> Int
nrSolutions3 n = sum $ fmap (countSolutions n) (validYSquares n)

solve3 :: Int
solve3 = length $ filter (== 10) $ fmap nrSolutions3 [1..999999]

main = print solve3     -- 4989


test3 = find (\n -> nrSolutions n /= nrSolutions3 n) [1..]



