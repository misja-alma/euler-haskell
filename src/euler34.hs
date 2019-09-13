import Data.Char (digitToInt)

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

fac 0 = 1
fac n = n * fac (n-1)

factorialSum n = sum $ map fac (toDigits n)

factorialSums = filter (\n -> n == factorialSum n) [3..]

main = print $ sum $ take 2 factorialSums