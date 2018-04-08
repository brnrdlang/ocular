{-# LANGUAGE RecordWildCards #-}

module Data.Array.Colour.LinearRGB (
    HDRImage,
    fromImage,
    toImage,
    toRGBF,
    sRGB,
    sRGBinv
    ) where

import qualified Codec.Picture as JP
import Data.Array.Repa

type HDRImage = Array D DIM3 Float

fromImage :: JP.DynamicImage -> HDRImage
fromImage (JP.ImageRGB8 img) = fromImageRGB img
fromImage (JP.ImageRGBF img) = fromImageHDR img
fromImage img = fromImageRGB (JP.convertRGB8 img)

takeOne :: (a, a, a) -> Int -> a
takeOne (x, _, _) 0 = x
takeOne (_, x, _) 1 = x
takeOne (_, _, x) 2 = x
takeOne _ _ = error "Index out of range."

fromImageRGB :: JP.Image JP.PixelRGB8 -> HDRImage
fromImageRGB img@JP.Image {..} = 
  fromFunction
    (Z :. imageHeight :. imageWidth :. 3)
    (\(Z :. y :. x :. c) ->
      let (JP.PixelRGB8 r g b) = JP.pixelAt img x y
      in sRGBinv (takeOne (fromIntegral r / 255.0,fromIntegral g / 255.0, fromIntegral b / 255.0) c) )

fromImageHDR :: JP.Image JP.PixelRGBF -> HDRImage
fromImageHDR img@JP.Image {..} =
  fromFunction
    (Z :. imageHeight :. imageWidth :. 3)
    (\(Z :. y :. x :. c) ->
      let (JP.PixelRGBF r g b) = JP.pixelAt img x y
      in sRGBinv (takeOne (r, g, b) c) )

toImage :: HDRImage -> JP.Image JP.PixelRGB8
toImage hdrimg = JP.generateImage gen width height
  where
    Z :. height :. width :. _ = extent hdrimg
    clamp v
        | v < 0.0 = 0
        | v <= 1.0 = round(255 * sRGB v)
        | otherwise = 255
    gen x y = 
        let r = clamp (hdrimg ! (Z :. y :. x :. 0))
            g = clamp (hdrimg ! (Z :. y :. x :. 1))
            b = clamp (hdrimg ! (Z :. y :. x :. 2))
        in JP.PixelRGB8 r g b

toRGBF :: HDRImage -> JP.Image JP.PixelRGBF
toRGBF hdr = JP.generateImage gen width height
  where
    Z :. height :. width :. _ = extent hdr
    gen x y =
      let r = hdr ! (Z :. y :. x :. 0)
          g = hdr ! (Z :. y :. x :. 1)
          b = hdr ! (Z :. y :. x :. 2)
      in JP.PixelRGBF r g b

sRGB :: Float -> Float
sRGB v
    | v <= 0.0031308 = 12.92 * v
    | otherwise = (1.0 + a) * v ** (1.0/2.4) - a
        where a = 0.055

sRGBinv :: Float -> Float
sRGBinv v
    | v <= 0.04045 = v / 12.92
    | otherwise = ( (v + a)/(1.0 + a) ) ** 2.4
        where a = 0.055
