-- Take the list of all primes. For each prime:
-- calculate all consecutive sumes starting with the prime, that are below one million, and are a prime. Take the max.
-- Take the max off all thus generated maxes.

import Data.List
import Data.Function
import qualified Data.Set as Set
import Control.Monad.Trans.State

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

smallPrimes = takeWhile (<10000) primes

isPrimeC :: (Set.Set Integer, Integer) -> (Set.Set Integer, Bool)
isPrimeC (s, i) | Set.member i s = (s, True)
                | otherwise = if isPrime i then (Set.insert i s, True)
                	                       else (s, False)

type PrimeCState = State (Set.Set Integer)

-- (>>=) :: (Set.Set Integer, Integer) -> (Integer -> (Set.Set Integer, Bool)) -> (Set.Set Integer, Bool)
-- (sp1 >>= f) s = f sp1 

maxPrimeSeriesEnd :: [Integer] -> Integer -> Integer -> Integer
maxPrimeSeriesEnd [] bestSoFar _ = bestSoFar
maxPrimeSeriesEnd (p:ps) bestSoFar sumSoFar = let nextSum = sumSoFar + p in
                    if nextSum >= 1000000 then bestSoFar
                                          else if isPrime nextSum then maxPrimeSeriesEnd ps p nextSum 
                                                   	              else maxPrimeSeriesEnd ps bestSoFar nextSum

primeSeries :: [Integer] -> [[Integer]]
primeSeries series@(p:ps) | null ps      = [[p]]
                          | otherwise    = series : primeSeries ps 

maxPrimeSum = maximumBy (compare `on` length) $ map (\ps -> takeWhile ( <= maxPrimeSeriesEnd ps 0 0) ps) (primeSeries smallPrimes)

-- TODO these are too many!
--primeSeries = filter (isPrime . sum) $ subsequences (takeWhile (<1000000) primes)

-- TODO now use a monad
main = do let s = Set.empty;
	          a = isPrimeC (s, 3) 
          print $ isPrimeC (fst a, 5) 
