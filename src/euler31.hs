-- Can be solved with backtracking; search until result is 200 (good) or > 200 (wrong)
-- iterate over a list of sublists; where each sublist represents a different coin 
-- continue taking from a sublist until up or over 200, then return to the situation with one less and try there the next one,
-- until that finishes, then the next one, and finally one more back again
-- the result is just a counter that counts the successes

coins = [1, 2, 5, 10, 20, 50, 100, 200]
totalToFind = 200

sumOverRemainingCoins coinSum coinIndex = sum $ map (doFindTotal coinSum) [coinIndex..(length coins - 1)] 

doFindTotal coinSum coinIndex | newSum == totalToFind = 1
                              | newSum > totalToFind  = 0
                              | otherwise             = sumOverRemainingCoins newSum coinIndex
                      		where newSum = coinSum + coins !! coinIndex

main = print $ sumOverRemainingCoins 0 0 