import Data.Char (digitToInt)
import Data.List

toDigits :: Integer -> [Int]
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

rotations xs = map (\n -> drop n xs ++ take n xs) [0..length xs - 1]

rotatedNumbers n = map fromDigits (rotations $ toDigits n)

isRotationPrime p = all isPrime $ rotatedNumbers p 

noSuspiciousDigits n = let digits = toDigits n in
                         n < 10 || null ([0, 2, 4, 5, 6, 8] `intersect` digits)

-- Note the optimization with noSuspiciousDigits. It has to come before isRotationPrime because its way less expensive

main = print $ length $ (filter isRotationPrime . filter noSuspiciousDigits) [2..999999]