module Vector4 where

import Data.Ix

data Vector4 = Vector4 !Int !Int !Int !Int
  deriving (Eq, Ord)

instance Show Vector4 where
  show (Vector4 w x y z) = show (w, x, y, z)

instance Ix Vector4 where
  range (Vector4 wl xl yl zl, Vector4 wu xu yu zu) =
    fromTuple <$> range ((wl, xl, yl, zl), (wu, xu, yu, zu))
  inRange (Vector4 wl xl yl zl, Vector4 wu xu yu zu) (Vector4 w x y z) =
    inRange ((wl, xl, yl, zl), (wu, xu, yu, zu)) (w, x, y, z)
  index (Vector4 wl xl yl zl, Vector4 wu xu yu zu) (Vector4 w x y z) =
    index ((wl, xl, yl, zl), (wu, xu, yu, zu)) (w, x, y, z)

fromTuple :: (Int, Int, Int, Int) -> Vector4
fromTuple (w, x, y, z) =
  Vector4 w x y z

zero :: Vector4
zero = Vector4 0 0 0 0

add :: Vector4 -> Vector4 -> Vector4
add (Vector4 w1 x1 y1 z1) (Vector4 w2 x2 y2 z2) =
  Vector4 (w1 + w2) (x1 + x2) (y1 + y2) (z1 + z2)

scale :: Int -> Vector4 -> Vector4
scale a (Vector4 w x y z) =
  Vector4 (a * w) (a * x) (a * y) (a * z)

instance Semigroup Vector4 where
  (<>) = add

instance Monoid Vector4 where
  mempty = zero

expandBounds :: Vector4 -> (Vector4, Vector4) -> (Vector4, Vector4)
expandBounds (Vector4 w x y z) (Vector4 wl xl yl zl, Vector4 wu xu yu zu) =
  let lower = Vector4 (min w wl) (min x xl) (min y yl) (min z zl)
      upper = Vector4 (max w wu) (max x xu) (max y yu) (max z zu)
   in (lower, upper)
