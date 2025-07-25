module Lib (convert) where

import System.FilePath (takeExtension)
import Data.Char (toLower)
import Codec.Picture

data ImageFormat = Jpeg | Png | Gif
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
    Just Gif  -> Codec.Picture.readGif filePath 
    Nothing   -> return $ Left $ "This format is not supported" ++ filePath

writeImageFile :: FilePath -> DynamicImage -> ImageFormat -> IO ()
writeImageFile outputPath img format = do
  case format of
    Jpeg -> Codec.Picture.saveJpgImage 80 outputPath img 
    Png  -> Codec.Picture.savePngImage outputPath img
    Gif  -> do 
      let _ = Codec.Picture.saveGifImage outputPath img 
      return ()

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
          writeImageFile outputPath img targetFormat
          putStrLn $ "Successfully converted: " ++ inputPath ++ " -> " ++ outputPath


