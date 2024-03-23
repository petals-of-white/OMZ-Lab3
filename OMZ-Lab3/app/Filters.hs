module Filters where
import           Codec.BMP       (parseBMP)
import           Data.List       (sort, transpose)
import           Data.List.Split (chunksOf, divvy)
import           Graphics.Gloss  (Picture, bitmapOfBMP)
import           Graphics.Image  hiding (map, transpose, zipWith)

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

-- | Фільтр Гауса
gaussPicture :: Double -> Image VS Y Word16 -> Picture
gaussPicture sigma = hipToGloss . normalize .  applyFilter (gaussianBlur sigma) . toDoubleI

-- | Оператор Превітта
prewittPicture ::Image VS Y Word16 -> Picture
prewittPicture = hipToGloss . normalize . prewittOperator . toDoubleI

-- | Зважений медіанний фільтр
weightedMedianPicture :: [[Int]]  -> (Int, Int) -> [Word16]-> Picture
weightedMedianPicture weights (_row, col) wordList =
    let filtered = Filters.weightedMedian weights matrix

    in
        hipToGloss $ normalize $ toDoubleI (fromLists (map (map PixelY) filtered) :: Image VS Y Word16)
    where matrix = chunksOf col wordList

hipToGloss :: (Array arr cs e, Array arr RGB Double, ToRGB cs e, Writable (Image arr RGB Double) BMP)
 => Image arr cs e -> Picture
hipToGloss hipImg =
    let bmp = either (error . show) id $ parseBMP $ encode BMP [] $ toImageRGB hipImg
    in bitmapOfBMP bmp
