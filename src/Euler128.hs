module Euler128 where

import qualified Data.Set as Set

-- Only top and top right tiles can have 3 primes, the rest not.
-- A ring increases 6 per iteration, tile numbers are cumulative, this gives 3r(r+1) + 2 as the number of a top tile on ring r.

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

divides x y = x `rem` y == 0

isPrime :: Integer -> Bool
isPrime x
  | x < 2     = False
  | otherwise = all (not . divides x) $ takeWhile (\d -> d * d <= x) primes
  
primeCache :: Set.Set Integer
primeCache = Set.fromList $ take 70000 primes
  
topIndices :: [Integer]
topIndices = [0..] >>= topAndRight

topAndRight ring = [2 + 3 * ring * (ring + 1), 1 + 3 * (ring + 1) * (ring + 2)]  
  
filteredOffsets :: [[Integer]]
filteredOffsets = zip tops topRights >>= (\(t,r) -> [t,r])

tops = fmap topOffsets [6,12..] 
 
topRights = fmap topROffsets [6,12..]

topOffsets perimeter = [2 * perimeter + 5, perimeter, perimeter + 1, prevPerimeter perimeter, 1, perimeter - 1]

topROffsets perimeter = let prevPer = prevPerimeter perimeter in
                        if perimeter == 6 then 
                          [perimeter + 4, perimeter + 5, perimeter + 6, prevPer + 5, 1, perimeter - 1] 
                        else 
                          [perimeter + 5, perimeter + 6, prevPer + 6, perimeter + prevPer - 1,  1, perimeter - 1]

prevPerimeter :: Integer -> Integer
prevPerimeter 6 = 1
prevPerimeter p = p - 6

pd :: [Integer] -> Int
pd offsets = length $ filter (`Set.member` primeCache) offsets

pd3s :: [Integer]
pd3s = 1 : [i | (i,offsets) <- zip topIndices filteredOffsets, pd offsets == 3]

main :: IO()
main = print $ pd3s !! (2000 - 1)
