{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Vector where

data Vector3 = Vector3 { x, y, z :: Double } deriving Show

infixl 6 .+.
(.+.) :: Vector3 -> Vector3 -> Vector3
(.+.) (Vector3 x0 y0 z0) (Vector3 x1 y1 z1) =
    Vector3 (x0+x1) (y0+y1) (z0+z1)

infixl 6 .-.
(.-.) :: Vector3 -> Vector3 -> Vector3
(.-.) (Vector3 x y z) (Vector3 x' y' z') =
    Vector3 (x-x') (y-y') (z-z')

infixl 7 .*.
class VectorMul a b c | a b -> c where
    (.*.) :: a -> b -> c
instance VectorMul Vector3 Vector3 Vector3 where
    (Vector3 x y z) .*. (Vector3 x' y' z') =
        Vector3 (x*x') (y*y') (z*z')
instance VectorMul Double Vector3 Vector3 where
    s .*. v = vecMap (*s) v
instance VectorMul Vector3 Double Vector3 where
    v .*. s = vecMap (*s) v

infixl 7 ./.
(./.) :: Vector3 -> Double -> Vector3
(./.) v s = v .*. (1 / s)

vecMap :: (Double -> Double) -> Vector3 -> Vector3
vecMap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

vecNeg :: Vector3 -> Vector3
vecNeg (Vector3 x y z) = Vector3 (-x) (-y) (-z)

vecLength :: Vector3 -> Double
vecLength = sqrt . vecSquareLength

vecSquareLength :: Vector3 -> Double
vecSquareLength (Vector3 x y z) = x^2 + y^2 + z^2

vecDot :: Vector3 -> Vector3 -> Double
vecDot (Vector3 x y z) (Vector3 x' y' z') =
    x*x' + y*y' + z*z'

vecCross :: Vector3 -> Vector3 -> Vector3
vecCross (Vector3 x y z) (Vector3 x' y' z') =
    Vector3 (y * z' - z * y')
            (z * x' - x * z')
            (x * y' - y * x')

vecNormalize :: Vector3 -> Vector3
vecNormalize v = v ./. vecLength v
