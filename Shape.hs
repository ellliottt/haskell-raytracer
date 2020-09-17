module Shape where

import Vector
import Ray

data Shape = Sphere { pos :: Vector3, radius :: Double }

-- check a ray for intersection with a Shape.
-- if hit, return the Just distance to the closest hit
-- else return Nothing.
intersect :: Ray -> Shape -> Maybe Double

-- TODO: derive this properly!
intersect (Ray org dir) (Sphere pos r) =
    let oc  = org .-. pos
        a   = vecDot dir dir
        b   = 2 * vecDot oc dir
        c   = (vecDot oc oc) - r^2
        dis = b^2 - 4 * a * c
    in if dis < 0
       then Nothing
       else Just $ (-b - (sqrt dis)) / (2 * a)

-- for a ray and a distance, get the surface normal
-- of the shape at that hit point.
normal :: Ray -> Double -> Shape -> Vector3

-- hit - center gives normal vector
normal ray dist (Sphere pos _) =
    vecNormalize $ (rayPos ray dist) .-. pos
