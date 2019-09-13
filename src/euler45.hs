triangular n = n * (n + 1) `quot` 2
triangulars = map triangular [1..]

pentagonal n = n * (3 * n - 1) `quot` 2
pentagonals = map pentagonal [1..]

hexagonal n = n * (2 * n - 1)
hexagonals = map hexagonal [1..]


-- intersect 2 infinite monotunously growing lists
intersectIM xs ys = let x = head xs
                        y = head ys in
                        if x < y then intersectIM (tail xs) ys
                                 else if x > y then intersectIM xs (tail ys)
                                               else x : intersectIM (tail xs) (tail ys) 

main = print $ take 3 $ intersectIM hexagonals $ intersectIM triangulars pentagonals