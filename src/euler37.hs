import Data.Char (digitToInt)
import Data.List

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

fromDigits :: [Int] -> Integer
fromDigits xs = fromReverseDigits $ reverse xs

fromReverseDigits :: [Int] -> Integer
fromReverseDigits ([x]) = toInteger x
fromReverseDigits (x:xs) = toInteger x + 10 * fromReverseDigits xs

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

isPrime' x (p : prms) 
			| p * p > x                  = True
			| (x `mod` p) == 0           = False  
			| otherwise                  = isPrime' x prms

isPrime x 
	| x < 2     = False
	| otherwise = isPrime' x primes		

leftTruncs xs = map (`drop` xs) [0..length xs - 1]

rightTruncs xs = map (`take` xs) [1..length xs]

truncNumbers n = let digits = toDigits n in
	                 map fromDigits $ leftTruncs digits `union` rightTruncs digits

isTruncPrime n = all isPrime $ truncNumbers n 

main = print $ sum $ take 11 $ filter isTruncPrime [11..]