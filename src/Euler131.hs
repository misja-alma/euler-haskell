module Euler131 where

-- n^3 + p*n^2 = q^3
-- say that q = n + d, writing out and simplifying then gives: p = 3d + 3d^2/n + d^3/n^2
-- We could iterate over d's or n's and check for each combi that n^2 divides n3d^2 + d^3
-- Since p is prime, it must be that n = a * d (a>1) otherwise p would be divisible by d.
-- This gives p = 3d + d(3/a + 1/a*a)
-- Since p is a whole number, d(3/a + 1/a*a) must be as well.
-- So we can write: d = xa*a/ (3a + 1). Again this needs to be a whole number. This means that:
-- 1. 3a + 1 is divisible by x, or:
-- 2. 3a + 1 is divisible by a*a. This is not possible. or:
-- 3. x = nq, a*a = rs, 3a + 1 is divisible by nr. (And skip all trivial cases.)
--    With other words: a*a and 3a + 1 are not coprime. However, it seems that this is always the case. Checked empirically, still lacks a proof.
-- So: 3a + 1 is divisible by x; Substituting gives d = n*a*a ! (n >= 1)
-- So p = 3naa + 3na + n = n(3aa + 3a + 1). But since p is prime, it can't be that n > 1; so p = 3aa + 3a + 1 !!

import Data.Numbers.Primes

allSolutions = takeWhile (< 1000000) $ fmap (\a -> 3 * a * a + 3 * a + 1) [1..]

main = print $ length $ filter isPrime allSolutions
