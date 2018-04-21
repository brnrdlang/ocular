{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeOperators       #-}

module Vision.Human.Algorithms (
  michelson_filter,
  anchoring,
  retinex,
  edge_integrator,
  edge_detect
  ) where

import Prelude hiding (map)

import Data.Array.Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

michelson_filter :: Array D DIM3 Float -> Array D DIM3 Float
michelson_filter image =
  mapLast mlsncon image
  where
    diff = [stencil2| -1 -1 -1
                      -1  8 -1
                      -1 -1 -1|]
    norm = [stencil2|  1  1  1
                       1  8  1
                       1  1  1|]
    mlsncon im = (mapStencil2 BoundClamp diff im) /^ (map (+1) . mapStencil2 BoundClamp norm $ im)

edge_detect :: Array D DIM3 Float -> Array D DIM3 Float
edge_detect image =
  mapLast edgefilter image
  where
    psf = [stencil2| -1 -1 -1
                     -1  8 -1
                     -1 -1 -1|]
    edgefilter = delay . mapStencil2 BoundClamp psf

mapLast :: Shape sh => (Array D sh a -> Array D sh b) -> Array D (sh :. Int) a -> Array D (sh :. Int) b
mapLast f arr = fromFunction (extent arr) reductor
  where
    reductor (sh :. x) = sliced ! sh
        where sliced = f $ slice arr (Any :. x)

edge_integrator :: Array r DIM3 Float -> Array r DIM3 Float
edge_integrator arr = arr

retinex :: Array r DIM3 Float -> Array r DIM3 Float
retinex arr = arr

anchoring :: Array r DIM3 Float -> Array r DIM3 Float
anchoring arr = arr
