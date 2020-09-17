module Ray where

import Vector

data Ray = Ray { origin, direction :: Vector3 } deriving Show

-- get the Vector3 position of a distance along a Ray
rayPos :: Ray -> Double -> Vector3
rayPos (Ray org dir) t = org .+. dir .*. t
