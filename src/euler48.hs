

selfPowers = map (\n -> n^n) [1..]

main = do
    let powerSum = show $ sum $ take 1000 selfPowers 
    print $ drop (length powerSum - 10) powerSum 