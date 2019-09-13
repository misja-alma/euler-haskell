import Data.List

-- map nrs to their distinct prime factors list length
-- find the 1st sequence of 4's with length 4.

divides a b = b `rem` a == 0

lowestDivider = ldf 2

ldf k n | divides k n = k
        | k * k > n   = n
        | otherwise   = ldf (k + 1) n  

factors n | n == 1    = []
          | otherwise = p : factors (div n p) where p = lowestDivider n

distinctFactorLength = length . nub . factors

-- We'd like to split up this list into sections where each section has a length, a value and a startIndex.
-- Or we could just do a searh for an element with factorlength 4, with 3 neighbours that have length 4 also. 

factorlength4With3Neighbours n = all (\f -> distinctFactorLength f == 4) [n..n+3]

main = print $ find factorlength4With3Neighbours [1..]
