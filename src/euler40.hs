-- the digits are every time a power of 10 apart. So we want a filter that filters those power indices from a list. Note that they are 1-based.
-- the argument of the powerfilter is just a lazy list consisting of infinitely many integers concatenated.
-- finally we do a take 7 from the power list.
-- but how to write the powerfilter in a clever way? Recursively?

import Data.List
import Data.Char (digitToInt)

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

powerFilter' xs i nextPower | null xs = []
                            | otherwise = if i == nextPower then head xs : powerFilter' (tail xs) (i+1) (nextPower*10)
                           	                                else powerFilter' (tail xs) (i+1) nextPower

powerFilter xs = powerFilter' xs 1 1

concatenatedNumbers = foldr ((++) . toDigits) [] [1..]

main = print . product . take 7 $ powerFilter concatenatedNumbers               	                                