-- Algorithm; take a pentagonal. Then get all lower pentagonals where the sum is a pentagonal. Then filter on the ones where the diff is a pentagonal;
-- And take the one with the lowest diff.
-- So we have a function which takes a pentagonal and returns Maybe a diff.
-- Then, iterate over all pentagonals. Take the diff from the func above. Iterate further only if the diff with the next pentagonal > the current max diff.

import Data.Maybe

pentagonal :: Int -> Int
pentagonal n = n * (3 * n - 1) `quot` 2

pentagonals = map pentagonal [1..]

isInt x = x == fromInteger (round x)

isPentagonalSmart :: Int -> Bool
isPentagonalSmart n = isInt ((sqrt (1 + 24 * fromIntegral n) + 1) / 6)

lowerPentagonals n = takeWhile (<n) pentagonals

isPentagonalDifSum n x = isPentagonalSmart (n + x ) && isPentagonalSmart (n - x)

lowerPentagonalDifs n = map (n -) $ filter (isPentagonalDifSum n) (lowerPentagonals n)

lowestPentagonalDif n = let lowerDifs = lowerPentagonalDifs n in
                            if null lowerDifs then Nothing else Just $ minimum lowerDifs

-- Just taking the first was already a solution, no need to iterate furter and check if the diff doenst get too big.
main = print $ take 1 $ mapMaybe lowestPentagonalDif pentagonals