-- Max n to consider; since multiplier has to be > 1, the pandigital will be composed of at least 2 products.
-- So n has to be <= 9999 because with 5 digits the result would be too long.

import Data.List
import Data.Char (digitToInt)
import Data.Maybe

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

fromDigits :: [Int] -> Integer
fromDigits xs = fromReverseDigits $ reverse xs

fromReverseDigits :: [Int] -> Integer
fromReverseDigits ([x]) = toInteger x
fromReverseDigits (x:xs) = toInteger x + 10 * fromReverseDigits xs

oneToNinePresent digits = length digits == 9 && null ([1..9] \\ digits)

isPanDigitalProduct' n multiplier digitsSoFar = let digits = digitsSoFar ++ toDigits (multiplier * n) in
                                                    if length digits < 9 then isPanDigitalProduct' n (multiplier + 1) digits
                                                   	                     else 
                                                   	                     	if length digits == 9 && multiplier > 1 && oneToNinePresent digits 
                                                   	                     		then
                                                   	                     		  Just digits
                                                   	                     		else
                                                   	                     		   Nothing  

isPanDigitalProduct n = isPanDigitalProduct' n 1 []

main = print $ maximum $ (map fromDigits . mapMaybe isPanDigitalProduct) [1..9999]

