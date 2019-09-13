import System.IO  
import Data.Char (ord)
import Data.List.Split (splitOn)

triangle :: Int -> Int
triangle n = quot (n* (n + 1)) 2
  
triangleNumbers :: [Int]  
triangleNumbers = map triangle [1..]

wordValue = sum . map (subtract (ord 'A' - 1) . ord) 

isTriangleWord w = let vw = wordValue w in
 	                   elem vw $ takeWhile (<= vw) triangleNumbers 

removeQuotes w = tail $ init w

toWords = map removeQuotes . splitOn "," 

main = do  
    handle <- openFile "p042_words.txt" ReadMode  
    contents <- hGetContents handle  
    let words = toWords contents
    print $ length $ filter isTriangleWord words  
    hClose handle 