import Data.Char (digitToInt)

-- The upperbound to look for can be found like this:
-- the highest digit is 9, and 9^5 = 59049.
-- So when a number is 6 digits, so above 100000, and we consider if we should look for numbers with 7 digits, the extra digit will start the one million range,
-- while it can only add as much as 59049 to the sum.
-- In fact, 9^5 * 6 is only 354294. So looking beyond that number is pointless.
-- The underbound is simply 2.

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

powerSum :: [Int] -> Int
powerSum = sum . map (^5)

numberIsSumOfPowers x = x == powerSum (toDigits x) 

powerNumbers = filter numberIsSumOfPowers

main = print $ sum $ powerNumbers [2 .. 6 * 9^5]