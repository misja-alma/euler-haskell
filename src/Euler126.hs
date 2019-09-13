module Euler126
    ( main
    ) where

import Data.List

main :: IO ()
main = putStrLn $ "Solution: " ++ show (minimum $ layersOfSize 20000 1000)

layerSize :: Int -> Int -> Int -> Int -> Int
layerSize perimeter surface depth n = 2 * sum (fmap slice [0 .. n-1]) + depth * slice n
  where slice = sliceSize perimeter surface

sliceSize :: Int -> Int -> Int -> Int
sliceSize perimeter surface 0 = surface
sliceSize perimeter surface n = perimeter + (n-1) * 4

data Cuboid = Cuboid {length:: Int, width:: Int, depth:: Int} deriving Show

surface :: Cuboid -> Int
surface (Cuboid length width depth) = 2 * (length * width + length * depth + depth * width)

layerSizesUntil :: Int -> Cuboid -> [(Cuboid, Int)]
layerSizesUntil maxSize cuboid =
  let (Cuboid length width depth) = cuboid
      surface = length * width
      perimeter = 2 * (length + width)
      layerSizes = fmap (\n -> (cuboid, layerSize perimeter surface depth n)) [1 ..] in
  takeWhile (\p -> snd p <= maxSize) layerSizes

cuboidsUntil :: Int -> [Cuboid]
cuboidsUntil maxSurface = do
      (Cuboid l1 _ _) <-  limitSurfaceSize [ Cuboid l 1 1 | l <- [1..]]
      (Cuboid l2 w2 _) <- limitSurfaceSize [ Cuboid l1 w 1 | w <- [1..l1]]
      limitSurfaceSize [ Cuboid l2 w2 d | d <- [1..w2]]
        where limitSurfaceSize = takeWhile (\c -> surface c <= maxSurface)

allLayersUntil :: Int -> [(Cuboid, Int)]
allLayersUntil maxSize = do
  cuboids <- cuboidsUntil maxSize
  layerSizesUntil maxSize cuboids

groupByLayerSize :: [(Cuboid, Int)] -> [[(Cuboid, Int)]]
groupByLayerSize xs = groupBy (\a b -> snd a == snd b) $ sortOn snd xs

layersOfSize :: Int -> Int -> [Int]
layersOfSize maxSurface size =
  let nrMembersByLayerSize = fmap (\c -> (Data.List.length c, snd $ head c)) $ groupByLayerSize $ allLayersUntil maxSurface in
    snd <$> filter (\c -> fst c == size) nrMembersByLayerSize
