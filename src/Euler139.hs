module Euler139 where

import qualified Data.Set as S

-- See: https://mathworld.wolfram.com/PythagoreanTriple.html
-- and: https://mathworld.wolfram.com/SumofSquaresFunction.html

-- so for m = 2 ...
-- for n = 1 .. m-1
-- if parity (m) \= parity(n)
-- primitive triple: c = m * m + n * n
-- and then take all multiples of c

-- Count all hypotenuse's:
-- generate all primitive c's as above;
-- then take all their multiples. Add to a map or array which has as value the count so far!
-- NOTE: we could check each of these with the problem check at the bottom while generating them!

-- Note that they are not quite ordered by c; hence the need for the limiting param maxPerimeter
-- Note 2: the perim in terms of m and n = m2 - n2 + 2mn + m2 + n2 = 2m2 + 2mn
primitiveTriples :: Int -> [(Int, Int, Int)]
primitiveTriples maxPerimeter = [(if a > b then b else a, if a > b then a else b, c)|
                                                                    m <- takeWhile (\m -> 2 * m * m + 2 * m <= maxPerimeter) [2..],
                                                                    let m2 = m * m,
                                                                    n <- takeWhile (\n -> 2 * m2 + 2 * m * n <= maxPerimeter) [1..m-1],
                                                                    even m /= even n,
                                                                    let n2 = n * n,
                                                                    let a = m2 - n2,
                                                                    let b = 2 * m * n,
                                                                    let c = m2 + n2]


-- NOTE There can be duplicates! So primitive1 * n1 == primitive2 * n2
allTriples :: Int -> [(Int, Int, Int)]
allTriples maxPerimeter = (primitiveTriples maxPerimeter) >>= (\(a, b, c) -> takeWhile (\(a2, b2, c2) -> a2 + b2 + c2 <= maxPerimeter) (fmap (\n -> (n * a, n * b, n * c)) [1..]))

-- NOTE this is slow, better to filer at the end
uniqueTriples maxPerimeter = S.fromList $ allTriples maxPerimeter

-- a*a + (a+n)*(a+n) = c * c; this means n has to be a prime of form +/- 1 mod 8
-- also, c has to be divisible by n
-- so for all triples; n = b - a; filter ones where n divides c
solutions maxPerimeter = filter (\(a, b, c) -> c `rem` (b - a) == 0) (allTriples maxPerimeter)

uniqueSolutions maxPerimeter = S.fromList $ solutions maxPerimeter

solve = length $ uniqueSolutions 99999999

main = print solve