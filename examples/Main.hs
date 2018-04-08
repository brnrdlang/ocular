module Main where

import System.Environment
import Codec.Picture


import Data.Array.Colour.LinearRGB

convert :: Either String DynamicImage -> Either String HDRImage
convert (Right img) = Right (fromImage img)
convert (Left s) = Left s

convertBack :: Either String HDRImage -> Either String (Image PixelRGB8)
convertBack (Right hdr) = Right (toImage hdr)
convertBack (Left s) = Left s

main :: IO ()
main = do
  args <- getArgs
  filename <- return (head args)
  img <- readImage filename
  hdr <- return (convert img)
  other_img <- return (convertBack hdr)
  putStrLn "Done."
