module Euler138 where


nextX x1 y1 n x y = x1 * x + n * y1 * y
nextY x1 y1 x y = x1 * y + y1 * x

pells x1 y1 n x y = let x' = nextX x1 y1 n x y
                        y' = nextY x1 y1 x y in
                    (x', y') : pells x1 y1 n x' y'

positivePellSolutions x1 y1 n = pells x1 y1 n x1 y1

evens (x:xs) = x:odds xs
evens _ = []

odds (_:xs) = evens xs
odds _ = []

-- only odd iterations are valid
negativePellSolutions x1 y1 n = odds $ pells x1 y1 n x1 y1

-- From n calculate m and from n + m calculate the sides a, b, c. We are only interested in c and we need the 12 smallest triangles (?)
-- negative fundamental: x1 = 2, y1 = 1, n = 5   (this gives m=4, n=1 which leads to the triangle in the example)
-- positive fundamental: x1 = 9, y1 = 4, n = 5
-- n = y, m = 2n + x
-- a,b,c come from Euler's equations
-- NOTE since the unfiltered neg. pell solution yields the positive solutions as well at the even indices, I could have just used that one ..

toTriangle (x, y) = let n = y
                        m = 2 * n + x in
                    (m * m - n * n, 2 * m * n, m * m + n * n)

posTriangles = (toTriangle (9, 4)) : (fmap toTriangle $ positivePellSolutions 9 4 5)

negTriangles = (toTriangle (2, 1)) : (fmap toTriangle $ negativePellSolutions 2 1 5)

mergeSorted :: [a] -> [a] -> (a -> a -> Bool) -> [a]
mergeSorted [] ys _ = ys
mergeSorted xs [] _ = xs
mergeSorted xxs@(x : xs) yys@(y : ys) selector = if (selector x y) then x : (mergeSorted xs yys selector) else y : (mergeSorted xxs ys selector)

triangles = mergeSorted posTriangles negTriangles (<)

solve = sum $ (\(_, _, c) -> c) <$> take 12 triangles
