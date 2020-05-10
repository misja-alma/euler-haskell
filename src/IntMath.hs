module IntMath (intSqrt) where

-- sqrtRounded is a bit faster than sqrtBinary, especially for larger numbers. sqrtHeron (equivalent to Newton's) is significantly slower.
intSqrt = sqrtRounded

sqrtBinary :: Int -> Maybe Int
sqrtBinary x = if x < 2 then Just x else guessBinary 1 x
  where guessBinary lower upper = let guess = (lower + upper) `div` 2
                                      sqr = guess * guess in
                          if sqr == x then Just guess else
                            if upper - lower <= 1 then Nothing else
                              if sqr > x then guessBinary lower guess else guessBinary guess upper

sqrtHeron :: Int -> Maybe Int
sqrtHeron x = if x < 2 then Just x else guessHeron (x `div` 2)
  where guessHeron guess = if guess * guess == x then Just guess else
                             let quotient = x `div` guess in
                             if abs (guess - quotient) < 2 then Nothing else guessHeron ((guess + quotient) `div` 2)

sqrtRounded :: Int -> Maybe Int
sqrtRounded x = let s = round $ sqrt $ fromIntegral x in
                if s * s == x then Just s else Nothing

-- when compiled with -O2, test sqrtRounded sqrtRounded takes 0.12 seconds.
test f1 f2 = all (\x -> f1 x == f2 x) [0..10000000]


