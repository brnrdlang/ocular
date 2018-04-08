module Data.Array.Colour.Radiance (
    RadianceImage(..),
    ) where

import Data.Array.Repa

data RadianceImage = RadianceImage { imgData :: Array D DIM3 Float
                         , spectrum :: Array U DIM1 Float
                         }
