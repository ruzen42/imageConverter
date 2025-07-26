module Main (main) where

import ImageConvert 
import Options.Applicative

data Options = Options 
  { sourcePath   :: FilePath
  , targetPath   :: FilePath 
  , maybeQuality :: Maybe Int
  }

main :: IO ()
main = do
  options <- execParser parserInfo 
  ImageConvert.convert (sourcePath options) (targetPath options) (maybeQuality options)

optionsParser = Options
  <$> argument str
      ( metavar "sourceFile"           
     <> help "Path to the input image file (e.g., image.jpg)" 
      )
  <*> argument str
      ( metavar "targetFile"           
     <> help "Path to the output image file (e.g., output.png)" 
      )
  <*> optional ( 
          option auto 
          ( long "quality" 
         <> short 'q'     
         <> metavar "INT"
         <> help "JPEG quality (0-100, default 80 if not specified)" 
         <> showDefault    
          )
      )

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper) 
  ( fullDesc                                   
 <> progDesc "A versatile command-line tool for converting image formats." 
 <> header "ImageConverter - Convert images between various formats"        
 <> footer "Examples:\n  image-converter input.jpg output.png\n  image-converter image.gif new_image.jpg" 
  )
