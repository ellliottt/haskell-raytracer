module Main where

import Data.Word

import Image
import Trace
import Vector
import Ray
import Shape

-- camera configuration
width  = 1920
height = 1080
aspRatio = width / height

vpHeight = 2
vpWidth = aspRatio * vpHeight
focalLen = 1

orig = Vector3 0 0 0
horiz = Vector3 vpWidth 0 0
vert = Vector3 0 vpHeight 0
lowerLeft = orig .-. (horiz ./. 2) .-. (vert ./. 2) .-. (Vector3 0 0 focalLen)

pixSize = vpHeight / height

-- list of shapes that define the scene
shapes = [ Sphere (Vector3 0 0 (-2)) 1 ]

-- run for each pixel of the screen.
-- computes the viewport Ray then calls `draw`.
drawPixel :: (Word32, Word32) -> Color
drawPixel (x, y) =
    let u = fromIntegral x / width :: Double
        v = (height - fromIntegral y) / height :: Double
        r = Ray orig (lowerLeft .+. (u .*. horiz) .+. (v .*. vert) .-. orig)
    in trace r shapes

main :: IO ()
main = makeImage "test.ff"
                 (fromIntegral $ round width, fromIntegral $ round height)
                 drawPixel

