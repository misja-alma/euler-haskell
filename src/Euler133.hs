module Euler133 where

import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Control.Monad.Extra
import Control.Monad.State.Lazy
import Data.Maybe

-- R(n) = 10^n - 1 / 9
-- R(10^n) = 10^10^n - 1 / 9
-- If a prime p divides R(10^n) it must divide 10^10^n - 1. IE it is modular to 10^x - 1 where x is a power of 10,
-- IE there exists an x for which 10^x mod p = 1
-- So: check all primes (that don't divide 9), determine there multiplicative order of 10 modulo p, and then check if that order divides some 10^n, 
-- IE if it is of the form 2^x*5^y so its distinct prime factors are (2,5)
-- If not, then it won't divide any R(10^n). Note that 2 and 5 have no multiplicative order since they are not coprime with 10 so they can be added to
-- the result immediately, and that 3 can be added as well because it does divide 10 - 1 but not any power of 10.

primes :: [Int]
primes = 2 : filter isPrime [3,5..99999]

divides :: Int -> Int -> Bool
divides x y = x `rem` y == 0

isPrime :: Int -> Bool
isPrime x
  | x < 2       = False
  | otherwise   = all (not . divides x) $ takeWhile (\d -> d * d <= x) primes

type DivisorsCache = Map.Map Int (Set.Set Int)

distinctPrimeFactorsCached :: Int -> State DivisorsCache (Set.Set Int)
distinctPrimeFactorsCached 1 = return Set.empty
distinctPrimeFactorsCached x = do
  cache <- get
  let c = cache Map.!? x
  ifM (return $ isNothing c) (
     do
       -- The internal updating of the cache doesn't speed up anything here, because we are calculating prime factors in ascending order; so everything lower is already known ..
       let (calcedVal, newCache) = let lowers = filter (divides x) $ takeWhile (\p -> p * p <= x) primes in
                                   if null lowers then (Set.singleton x, cache) else let (highers, cache') = runState (mapM (\l -> distinctPrimeFactorsCached (x `div` l)) lowers) cache in
                                                                                     (Set.unions (Set.fromList lowers : highers), cache')
       put $ Map.insert x calcedVal newCache
       return calcedVal ) (
     return $ fromJust c )
               
divides10Power :: Int -> State DivisorsCache Bool                                                                                
divides10Power x = let pFactors = distinctPrimeFactorsCached x in
                   (\fs -> Set.isSubsetOf fs (Set.fromList [2, 5])) <$> pFactors  
                                                                               
-- TODO implement a more efficient mult.order algo. Right now it is O(b)
-- Returns the multiplicative order of a modulo b. Requires that a is coprime to b.
multOrder :: Int -> Int -> Int
multOrder a b = 1 + length ( takeWhile (/= 1) $ iterate (\y -> (y * a) `rem` b) (a `rem` b) )

nonFactors = filterM (\p -> not <$> divides10Power (multOrder 10 p)) (drop 3 primes)

main = print $ sum [2,3,5] + sum (evalState nonFactors Map.empty)
