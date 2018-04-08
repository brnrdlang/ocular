module Main where

import System.Environment
import Codec.Picture
import Data.Array.Repa

import Data.Array.Colour.LinearRGB

process :: HDRImage -> HDRImage
--process img = Data.Array.Repa.map (\x -> 0.5*x+0.25) img
process img = fromFunction (extent img) flipImg
  where
    (Z :. h :. _ :. 3) = extent img
    flipImg (Z :. y :. x :. c) = img ! (Z :. h-y-1 :. x :. c)

main :: IO ()
main = do
  args <- getArgs
  filename <- return (head args)
  input_img <- readImage filename
  case input_img of
    Left s -> putStrLn s
    Right img ->
      let
        hdr = process (fromImage img)
        other_img = toImage hdr
      in writePng "flipped.png" other_img
