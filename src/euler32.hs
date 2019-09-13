-- We only have to consider numbers up to 4 digits. A 6 digit number's product would at least be 3 x 2 digits, together that's 11. And we have only the digits 1..9
-- A five number product can still not be made from 2 x 2 ditits; 98 * 76 is only 7448.
-- One way to solve it: take all numbers with distinct digits and check all their dividors. The dividor 1 can be skipped since it will duplicate the number's digits. 
-- For each packet, take their digits together, remove duplicates, sort, and then the highest digit should equal the original nr of digits of the packet.
-- Or actually there's a nicer way to do that; take the complement of the digits of the packet and the list [1..n] where n is the nr of digits. The complement should be empty. 
import Data.List
import Data.Char (digitToInt)
import Test.HUnit

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

listToDigits :: [Int] -> [Int]
listToDigits = concatMap toDigits

listToDigitsTest = TestCase $ assertEqual "should map list of numbers to list of their digits" [1,1,1,2] $ listToDigits [11, 12]

oneToNinePresent a (b, c) = let digits = listToDigits [a, b, c] in
                              length digits == 9 && null ([1..9] \\ digits)

dividers n = [(x, quot n x) | x <- [1..n], x * x < n, rem n x == 0]

isPandigital n = any (oneToNinePresent n) (dividers n)

main = print $ sum $ filter isPandigital [2..9999]