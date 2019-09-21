module Euler134 where

import Data.Numbers.Primes

-- modInverse using a forward rolling version of Euclid's extended algorithm. Requires that x is coprime with m.
modInverse :: Int -> Int -> Int
modInverse m x = let nextDivision (x1, x2, y1, y2, a, b) = let q = a `div` b
                                                               r = a `mod` b
                                                               x = x2 - q * x1
                                                               y = y2 - q * y1 in
                                                           (x, x1, y, y1, b, r)
                     (_, x2, _, _, _, _) = head $ dropWhile (\(_, _, _, _, _, b) -> b > 0) $ iterate nextDivision (0, 1, 1, 0, x, m) in
                 x2 `mod` m

nextP10 :: Int -> Int
nextP10 x = head $ dropWhile (<= x) $ iterate (* 10) 10

primePairConnection p1 p2 = let next10 = nextP10 p1
                                inverse =  modInverse next10 p2
                                factor = p1 * inverse `mod` next10 in
                            p2 * factor

connections = let primesFrom5 = drop 2 primes
                  pairs = zip (takeWhile (< 1000000) primesFrom5) (tail primesFrom5) in
              fmap (uncurry primePairConnection) pairs

main = print $ sum connections