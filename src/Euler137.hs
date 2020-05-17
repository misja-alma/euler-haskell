module Euler137 where

import Data.Maybe

sqrtRounded :: Integer -> Maybe Integer
sqrtRounded x = let s = round $ sqrt $ fromIntegral x in
                if s * s == x then Just s else Nothing
isSquare :: Integer -> Bool
isSquare x = isJust (sqrtRounded x)

-- Works but too slow
fs :: [Integer]
fs = (\y -> (y - 1) `div` 5) <$> filter (\y -> isSquare ((y * y + 4) `div` 5)) [toInteger 11,toInteger 16..]

-- Fast but no idea why this recurrence relation holds
fsi :: Int -> Integer
fsi 0 = 0
fsi 1 = 2
fsi n = 7 * fsi (n - 1) - fsi (n - 2) + 1

main = print $ fsi 15
