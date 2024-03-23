module Main where

import           Graphics.Gloss

import           Data.DICOM           (readObjectFromFile)
import           Data.DICOM.Utilities
import           Data.Word            (byteSwap16)
import           System.Environment   (getArgs)

import           App
import           Data.List.Split      (chunksOf)
import           Filters
import           Graphics.Image       (Array (fromLists), Pixel (PixelY))
import           Text.Read            (readMaybe)


data ProgramInput = ProgramInput {
  dicomFile     :: FilePath,
  medianWeights :: [[Int]],
  sigma         :: Double
}

parseArgs :: [String] -> Either String ProgramInput
parseArgs [] = Left "No arguments"
parseArgs [_] = Left "No sigma"
parseArgs (dicom:sigma:weights) = do
  sigm <- maybeToEither "Error reading sigma" $ readMaybe sigma
  kernel <- maybeToEither "Error reading kernel" $ readMaybe $ unwords weights
  return ProgramInput {dicomFile=dicom, medianWeights=kernel, sigma=sigm}

main :: IO ()
main = do
    args <- getArgs
    print $ unwords $ drop 2 args
    let ProgramInput{dicomFile=dicomPath, medianWeights=weights, sigma} = either error id $ parseArgs args
    print weights
    dicom <- either error id <$> readObjectFromFile dicomPath
    let elemMap = toMap dicom
        metadata = do
          r <- rows elemMap
          c <- columns elemMap
          intercept <- rescaleIntercept elemMap
          slope <- rescaleSlope elemMap
          bitsAlloc <- bitsAllocated elemMap
          imgBytes <- pixelData elemMap
          return (r,c, imgBytes, intercept, slope, bitsAlloc)

    case metadata of
      Right (row, col, imgBytes, intercept, slope, bitsAlloc) -> do
        case (intercept, slope, bitsAlloc) of
          (i, s, _) | i /= 0, s /= 0 -> do
            putStrLn "Texture is float"
            error "Expected Word16 Texture. Sorry"
          (_,_, 8)                   -> do
            putStrLn "Texture is Word8"
            error "Expected Word16 texture. Sorry"
          (_,_, 16)                  -> do
            putStrLn "Texture is Word16"
            let
                asWord16 = map byteSwap16 $ bsImgDicomToList (row, col) imgBytes

                (rowI, colI) = (fromIntegral row, fromIntegral col)
                asMatrix = chunksOf colI asWord16

                origHip = fromLists $ map (map PixelY) asMatrix
                originalPic = ogPicture (rowI, colI) asWord16

                gaussPic = gaussPicture sigma origHip
                prewittPic = prewittPicture origHip

                weightMedianPic = weightedMedianPicture weights (rowI, colI) asWord16

                change = handleMode originalPic gaussPic prewittPic weightMedianPic

            play (InWindow "Lab3" (rowI, colI) (10, 10)) white 1 Original change handleInput (const id)

          _                          ->
            error "Unrecognized pixel type. Expected Word16"

      Left _ -> error "Error reading metadata!"


