module Lib where

import System.FilePath (takeExtension)
import Data.Char (toLower)
import Codec.Picture
import Codec.Picture.Types

data ImageFormat = Jpeg | Png | Svg | Gif
  deriving (Show, Eq)

getFormat :: FilePath -> Maybe ImageFormat 
getFormat filePath = 
  case map toLower (takeExtension filePath) of
    ".jpg"  -> Just Jpeg 
    ".jpeg" -> Just Jpeg 
    ".png"  -> Just Png
    ".gif"  -> Just Gif
    _       -> Nothing 

readImageFile :: FilePath -> IO (Either String DynamicImage)
readImageFile filePath = do
  case getFormat filePath of 
    Just Jpeg -> Codec.Picture.readJpeg filePath 
    Just Png  -> Codec.Picture.readPng filePath 
    Nothing   -> return $ Left $ "This format is not supported" ++ filePath

writeImageFile :: FilePath -> DynamicImage -> ImageFormat -> IO (Either String ())
writeImageFile outputPath img format = do
  result <- case format of
    Jpeg -> Codec.Picture.saveJpgImage 80 outputPath img 
    Png  -> Codec.Picture.savePngImage outputPath img
  return result

convert :: FilePath -> FilePath -> IO ()
convert inputPath outputPath = do
  putStrLn $ "Attempting to convert " ++ inputPath ++ " to " ++ outputPath ++ "..."

  let maybeTargetFormat = getFormat outputPath
  case maybeTargetFormat of
    Nothing -> putStrLn $ "Error: Unsupported target file format: " ++ outputPath
    Just targetFormat -> do
      eimg <- readImageFile inputPath
      case eimg of
        Left err -> putStrLn $ "Error reading source image: " ++ err
        Right img -> do
          eWriteResult <- writeImageFile outputPath img targetFormat
          case eWriteResult of
            Left err -> putStrLn $ "Error writing image: " ++ err
            Right _  -> putStrLn $ "Successfully converted: " ++ inputPath ++ " -> " ++ outputPath


