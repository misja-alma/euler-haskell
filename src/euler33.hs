-- how to find the canceling fractions:
-- for each ab/cd where ab < cd; not both b and d are 0; and ab and cd have at least 1 digit in common; a and c are > 0
-- for each top/bottom match (max 4?), check that the result is the same fraction. If so, add ab/cd
-- finally, multiply all results and normalize the product.
import Data.Char (digitToInt)
import Data.List
import Data.Ratio

toDigits :: Int -> [Int]
toDigits = map digitToInt . show
 
--  reducing and comparing fractions will be done automatically!

sameFractions (a,b) (c,d) = a % b == c % d 

toFraction (a,b) = a % b

fractions = [ (a, b) | a <- [10..99], b <- [10..99], a < b, not (rem a 10 == 0 && rem b 10 == 0)]

-- The nice thing about 2-digit numbers is that we dont have to count both possibly canceled digits; the result is each time the same
withCanceledDigit :: [Int] -> [Int] -> Int -> [(Int, Int)]
withCanceledDigit xs ys y = let xsWithouty = xs \\ [y] 
                                ysWithouty = ys \\ [y] in
                              if length xsWithouty == length xs || length ysWithouty == length ys || fromReverseDigits ysWithouty == 0
                              	then [] 
                              	else [(fromDigits xsWithouty, fromDigits ysWithouty)] 

withCanceledDigits (a, b) = let as = toDigits a
                                bs = toDigits b in 
	                           concatMap (withCanceledDigit as bs) (nub bs)

sameWithCanceledDigits t = any (sameFractions t) (withCanceledDigits t) 

specialFractions = filter sameWithCanceledDigits fractions

main = print specialFractions --print $ product $ map toFraction specialFractions