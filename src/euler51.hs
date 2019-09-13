import Data.Char (digitToInt)
import Data.List

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

fromDigits :: [Int] -> Int
fromDigits xs = fromReverseDigits $ reverse xs

fromReverseDigits :: [Int] -> Int
fromReverseDigits ([x]) = x
fromReverseDigits (x:xs) = x + 10 * fromReverseDigits xs

primes :: [Int]
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

allReplacements :: Int -> Int -> [Int]
allReplacements n digit 
    | digit == 0 = map fromDigits $ all0Replacements (toDigits n) digit  
    | otherwise  = map fromDigits $ allReplacements' (toDigits n) digit 

-- replace all but the 1st nr. Empty list when length n is 0
all0Replacements :: [Int] -> Int -> [[Int]]
all0Replacements ([n]) digit  = []
all0Replacements (n:ns) digit = let tailReplaced = allReplacements' ns digit 
                                 in map (n :) tailReplaced     

allReplacements' :: [Int] -> Int -> [[Int]]
allReplacements' ([n]) digit  = [[digit]]
allReplacements' (n:ns) digit = (digit:ns) : (map (n :) (allReplacements' ns digit)) ++ (map (digit :) (allReplacements' ns digit))  

replacementPrimes n digit = filter isPrime $ allReplacements n digit


-- Wrong idea: we need to check replacement by replacement for all the digits if they are prime (and not start with 0), 
-- and then count the total over all the digits. And then take the next replacement etc.    
main = print $ replacementPrimes 123 0
