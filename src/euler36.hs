import Data.Char (digitToInt, intToDigit)
import Numeric
import Data.List

toDigits :: Int -> [Int]
toDigits = map digitToInt . show

isPalindrome xs = xs == reverse xs

toBinaryString n = showIntAtBase 2 intToDigit n ""

toBinaryDigits = map digitToInt . toBinaryString

isDoubleBasePalindrome n = isPalindrome (toDigits n) && isPalindrome (toBinaryDigits n)

main = print $ sum $ filter isDoubleBasePalindrome [1..999999] 