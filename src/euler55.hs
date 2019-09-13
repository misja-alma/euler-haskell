import Data.List

pascalRowStr r = concat (unwords (map show (pascalRow r))) "\n"

pascalRow :: Int -> [Int]
pascalRow r = map (nOverM r) [0 .. r]

nOverM :: Int -> Int -> Int
nOverM n m = fac n `div` (fac m * fac (n - m))

fac :: Int -> Int
fac n
    | n == 0 = 1
    | n < 0 = error "too small n!"
    | otherwise = n * fac (n - 1)

main :: IO ()
main = putStr $ show $ pascalRow 4

