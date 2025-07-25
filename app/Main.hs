module Main (main) where

import ImageConvert 
import Options.Applicative

data Options = Options 
  { sourcePath :: FilePath
  , targetPath :: FilePath 
  }

main :: IO ()
main = do
  opts <- execParser parserInfo 
  ImageConvert.convert (sourcePath opts) (targetPath opts)


optionsParser = Options
  <$> argument str
      ( metavar "SOURCE"           -- Отображаемое имя в справке (help message)
     <> help "Path to the input image file (e.g., image.jpg)" -- Описание аргумента
      )
  <*> argument str
      ( metavar "TARGET"           -- Отображаемое имя в справке
     <> help "Path to the output image file (e.g., output.png)" -- Описание аргумента
      )

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper) 
  ( fullDesc                                   
 <> progDesc "A versatile command-line tool for converting image formats." 
 <> header "ImageConverter - Convert images between various formats"        
 <> footer "Examples:\n  image-converter input.jpg output.png\n  image-converter image.gif new_image.jpg" 
  )
