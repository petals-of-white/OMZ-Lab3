module App where
import qualified Data.Binary                      as Binary
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.List.Split                  (chunksOf)
import           Data.Word                        (Word16)
import           Graphics.Gloss.Interface.IO.Game
import           Graphics.Image                   (Array (fromLists), Image,
                                                   Pixel (PixelY), VS, Y,
                                                   normalize, toDoubleI)

import           Filters                          (hipToGloss)



data AppImages pic = AppImages {original :: pic, gauss :: pic, prewitt :: pic}
data FilterMode = Original | Gauss | Prewitt | WeightenedMedian

ogPicture :: (Int, Int) -> [Word16] -> Picture
ogPicture (_, col) wordList =
    hipToGloss $ normalize $ toDoubleI (fromLists (map (map PixelY) matrix) :: Image VS Y Word16)
    where matrix = chunksOf col wordList

handleInput :: Event -> FilterMode -> FilterMode
handleInput (EventKey (Char 'o') Down _ _) _ = Original
handleInput (EventKey (Char 'p') Down _ _) _ = Prewitt
handleInput (EventKey (Char 'g') Down _ _) _ = Gauss
handleInput (EventKey (Char 'm') Down _ _) _ = WeightenedMedian
handleInput _ mode                           = mode

handleMode :: Picture -> Picture -> Picture -> Picture -> FilterMode -> Picture
handleMode og gauss prewitt wMedian mode =
    case mode of
        Original         -> og
        Gauss            -> gauss
        Prewitt          -> prewitt
        WeightenedMedian -> wMedian
bsImgDicomToList :: Binary.Binary a => (Word16, Word16) -> BS.ByteString -> [a]
bsImgDicomToList (row, col) =
    Binary.decode . LBS.append (Binary.encode imgLen)  . LBS.fromStrict
  where imgLen = fromIntegral row * fromIntegral col :: Int

-- buildImages :: (Int, Int) -> [Word16] ->

