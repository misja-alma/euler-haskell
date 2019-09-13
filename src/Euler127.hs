module Euler127 ( solve ) where

import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Control.Monad.Extra
import Control.Monad.State.Lazy
import Data.Maybe

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

divides :: Int -> Int -> Bool
divides x y = x `rem` y == 0

isPrime :: Int -> Bool
isPrime x
  | x < 2     = False
  | otherwise = all (not . divides x) $ takeWhile (\d -> d * d <= x) primes

type DivisorsCache = Map.Map Int (Set.Set Int)

distinctPrimeFactorsUncached :: Int -> Set.Set Int
distinctPrimeFactorsUncached x = 
  let lowers = filter (divides x) $ takeWhile (\p -> p * p <= x) primes in
                                     if null lowers then Set.singleton x else let highers = map (\l -> distinctPrimeFactorsUncached (x `div` l)) lowers in
                                                                               Set.unions (Set.fromList lowers : highers)

radicalsUncached :: Int -> Map.Map Int Int
radicalsUncached until = Map.fromAscList $ zip [1..until-1] (product <$> map distinctPrimeFactorsUncached [1..until-1])

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

radicalsCached :: Int -> Map.Map Int Int
radicalsCached until = Map.fromAscList $ zip [1..until-1] (product <$> evalState (mapM distinctPrimeFactorsCached [1..until-1]) Map.empty)

-- coPrimes plus 1 really
coPrimes :: Int -> [Int]
coPrimes x = filter (\y -> gcd x y == 1) [1..x-1]

-- TODO the nr of distinct prime factors is quite low below 120K so it would be much faster to start with selecting radicals.
abcHits :: Int -> [(Int,Int,Int)]
abcHits until = let cache = radicalsCached until in 
        do
          (b, productB) <- filter (\(b, pb) -> pb < b) $ takeWhile (\(b, _) -> b <= until - 2) $ Map.assocs cache  
          (a, productA) <- filter (\(a, pa) -> b + a < until && pa * productB < b) $ (\x -> (x, cache Map.! x)) <$> coPrimes b
          let c = a + b
              productC = cache Map.! c
              -- if gcd(a,b) == 1 it means gcd(a,a+b) and gcd(b,a+b) are also 1.
              radicalABC = productA * productB * productC
          [(a, b, c) | radicalABC < c]

trd :: (a,a,a) -> a
trd (_,_,x) = x

solve :: Int -> Int
solve = sum . fmap trd . abcHits

  


