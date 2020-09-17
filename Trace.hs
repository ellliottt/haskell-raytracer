module Trace where

import Vector
import Image
import Ray
import Shape

import qualified Data.List as L
import Data.Maybe
import Data.Bifunctor

-- interpolate between blue and white based on
-- Ray y component.
background :: Ray -> Color
background (Ray org dir) =
    let (Vector3 sx sy sz) = vecNormalize dir
        t = 0.5 * (sy + 1)
        (Vector3 r g b) = (1 - t) .*. (Vector3 1 1 1) .+. t .*. (Vector3 0.5 0.7 1)
    in unitColor r g b

-- trace a given Ray and find its Color.
-- map `intersect` over each shape, filter for
-- `Just` values, take the minimum distance.z
--     ints :: [(Shape, Maybe Double)]
--     hits :: [(Shape, Double)]
trace :: Ray -> [Shape] -> Color
trace ray shapes =
    let ints = zip shapes $ map (intersect ray) shapes
        hits = map (second fromJust) (filter (isJust . snd) ints)
    in case hits of
        [] -> background ray
        _  -> let (s, dist) = minOn snd hits
                  norm = normal ray dist s
              in unitColor (0.5 * (x norm + 1))
                           (0.5 * (y norm + 1))
                           (0.5 * (z norm + 1))

    where minOn f = head . L.sortOn f
