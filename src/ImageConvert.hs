module ImageConvert (convert) where

import System.FilePath (takeExtension)
import Data.Char (toLower)
import Codec.Picture
import Data.Maybe (fromMaybe)

data ImageFormat = Jpeg | Png | Gif | Tiff
  deriving (Show, Eq)

getFormat :: FilePath -> Maybe ImageFormat 
getFormat filePath = 
  case map toLower (takeExtension filePath) of
    ".jpg"  -> Just Jpeg 
    ".jpeg" -> Just Jpeg 
    ".png"  -> Just Png
    ".gif"  -> Just Gif
    ".tiff"  -> Just Tiff
    _       -> Nothing 

readImageFile :: FilePath -> IO (Either String DynamicImage)
readImageFile filePath = do
  case getFormat filePath of 
    Just Jpeg -> Codec.Picture.readJpeg filePath 
    Just Png  -> Codec.Picture.readPng filePath 
    Just Gif  -> Codec.Picture.readGif filePath 
    Nothing   -> return $ Left $ "This format is not supported" ++ filePath

writeImageFile :: FilePath -> DynamicImage -> ImageFormat -> Maybe Int -> IO ()
writeImageFile outputPath img format maybeQuality = do
  let quality = fromMaybe 80 maybeQuality
  case format of
    Jpeg -> Codec.Picture.saveJpgImage quality outputPath img 
    Png  -> Codec.Picture.savePngImage outputPath img
    Tiff -> Codec.Picture.saveTiffImage outputPath img
    Gif  -> do 
      let result = Codec.Picture.saveGifImage outputPath img 
      return ()

convert :: FilePath -> FilePath -> Maybe Int -> IO ()
convert inputPath outputPath quality = do
  let maybeTargetFormat = getFormat outputPath
  case maybeTargetFormat of
    Nothing -> putStrLn $ "Error: Unsupported target file format: " ++ outputPath
    Just targetFormat -> do
      eimg <- readImageFile inputPath
      case eimg of
        Left err  -> putStrLn $ "Error reading source image: " ++ err
        Right img -> do
          writeImageFile outputPath img targetFormat quality
          putStrLn $ "Successfully converted: " ++ inputPath ++ " -> " ++ outputPath


