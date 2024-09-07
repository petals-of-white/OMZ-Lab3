module App where
import           Codec.BMP                        (parseBMP)
import qualified Data.Binary                      as Binary
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.List.Split                  (chunksOf)
import qualified Filters
import           Graphics.Gloss                   (bitmapOfBMP)
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Image                   hiding (map)


data FilterMode = Original | Gauss | Prewitt | WeightedMedian

data AppImages pic = AppImages {original :: pic, gauss :: pic, prewitt :: pic, weightedMedian :: pic} deriving Functor

-- | Generate static images from Word 16 DICOM pixel data. Image are converted to Double for convenience
staticImgFromPixData :: (Word16, Word16) -> BS.ByteString -> Double -> [[Int]] -> AppImages (Image VS Y Double)
staticImgFromPixData sz@(_row, col) pixData sigma weights =
    let og = toDoubleI $ fromLists matrixPixels in
    AppImages {
        original = og,
        gauss = applyFilter (gaussianBlur sigma) og,
        prewitt = prewittOperator og,
        weightedMedian = toDoubleI $ fromLists $ Filters.weightedMedian weights matrixPixels
    }
    where
        flatPixels = map Binary.byteSwap16 $ dicomPixDataToList sz pixData :: [Word16]
        matrixPixels =chunksOf colI $ map PixelY flatPixels
        colI = fromIntegral col


-- | Keyboard events
handleInput :: Event -> FilterMode -> FilterMode
handleInput (EventKey (Char 'o') Down _ _) _ = Original
handleInput (EventKey (Char 'p') Down _ _) _ = Prewitt
handleInput (EventKey (Char 'g') Down _ _) _ = Gauss
handleInput (EventKey (Char 'm') Down _ _) _ = WeightedMedian
handleInput _ mode                           = mode

handleMode :: Picture -> Picture -> Picture -> Picture -> FilterMode -> Picture
handleMode og gauss prewitt wMedian mode =
    case mode of
        Original       -> og
        Gauss          -> gauss
        Prewitt        -> prewitt
        WeightedMedian -> wMedian

dicomPixDataToList :: Binary.Binary a => (Word16, Word16) -> BS.ByteString -> [a]
dicomPixDataToList (row, col) =
    Binary.decode . LBS.append (Binary.encode imgLen)  . LBS.fromStrict
  where imgLen = fromIntegral row * fromIntegral col :: Int


hipToGloss ::
    (Array arr cs e, Array arr RGB Double, ToRGB cs e, Writable (Image arr RGB Double) BMP) =>
    Image arr cs e -> Picture

hipToGloss hipImg =
    let bmp = either (error . show) id $ parseBMP $ encode BMP [] $ toImageRGB hipImg
    in bitmapOfBMP bmp
