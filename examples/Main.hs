module Main where

import System.Environment
import Codec.Picture

import Data.Array.Colour.LinearRGB
import Vision.Human.Algorithms

import qualified Data.Array.Repa as RP
main :: IO ()
main = do
  args <- getArgs
  filename <- return (head args)
  input_img <- readImage filename
  case input_img of
    Left s -> putStrLn s
    Right img ->
      let
        hdr = RP.map ((0.5+) . (0.5*)) $ michelson_filter $ fromImage img
        srgb = toImage hdr
      in writePng "michelson.png" srgb
  case input_img of
    Left s -> putStrLn s
    Right img ->
      let
        hdr = RP.map ((0.5+) . (0.5*)) $ edge_detect $ fromImage img
        srgb = toImage hdr
      in writePng "simple_edge.png" srgb
