-- Filters not implemented in HIP library, but required for lab assignment
module Filters where
import           Data.List       (sort, transpose)
import           Data.List.Split (divvy)

listMedian :: Ord a => [a] -> a
listMedian = (\sorted -> sorted !! (length sorted `div` 2)) . sort

windowed2D :: Int -> [[a]] -> [[[[a]]]]
windowed2D radius =
   map (transpose . map (divvy n 1)) . divvy n 1
   where n = radius * 2 + 1

-- | Звичайний медіанний фільтр
median :: Ord a => Int -> [[a]] -> [[a]]
median radius =
   map (
        map (listMedian . concat)
    )
   . windowed2D radius

-- | Зважений медіанний фільтр
weightedMedian :: Ord a => [[Int]] -> [[a]] -> [[a]]
weightedMedian weights =
    map (
        map (listMedian  . concat . zipWith replicate flattenedWeights  . concat)
    )
    . windowed2D radius
    where
        radius = length weights `div` 2
        flattenedWeights = concat weights
