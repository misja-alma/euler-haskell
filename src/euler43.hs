-- from right to left; multiples of 17, 13, 11 etc. After the 17s the next nr can only add 1 nr which is limited because of the pandigital constraint.
-- things stop after multiple 2; at that point the nr should be pandigital except 1 nr but that nr should not be 0.

import Data.List

multiples n = takeWhile (< 1000) (map (*n) [0..])

pad3Zeroes s = replicate (3 - length s) '0' ++ s
                  
stringMultiples n = map (pad3Zeroes .  show) $ multiples n

isDistinct xs = length xs == length (nub xs)

isDivisableBy numerator divider = numerator `rem` divider == 0

nextCandidates digitsSoFar = filter isDistinct $ map (: digitsSoFar) (['0'..'9'] \\ digitsSoFar)

startWith17 = filter isDistinct $ stringMultiples 17

isStartDivisibleBy n xs = read (take 3 xs) `isDivisableBy` n

-- Too lazy to generalize this
findNextPrime n 
     | n == 17 = 13
     | n == 13 = 11
     | n == 11 = 7
     | n == 7 = 5
     | n == 5 = 3
     | otherwise = 2

expandSolutions lastPrime solutions 
     | lastPrime == 2 = solutions
     | otherwise = let nextPrime = findNextPrime lastPrime 
                       solutionsForNextPrime = filter (isStartDivisibleBy nextPrime) $ concatMap nextCandidates solutions in
                       expandSolutions nextPrime solutionsForNextPrime 

-- Thankfully the missing nr was never zero so no need to filter those out
prependMissingNumber xs = let missingNumber = head $ ['0'..'9'] \\ xs in
                              missingNumber : xs

main = print $ sum $ map (read . prependMissingNumber) $ expandSolutions 17 startWith17 