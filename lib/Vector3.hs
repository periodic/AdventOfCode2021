module Vector3 where

import Data.Ix

data Vector3 = Vector3 !Int !Int !Int
  deriving (Eq, Ord)

instance Show Vector3 where
  show (Vector3 x y z) = show (x, y, z)

instance Ix Vector3 where
  range (Vector3 xl yl zl, Vector3 xu yu zu) =
    fromTuple <$> range ((xl, yl, zl), (xu, yu, zu))
  inRange (Vector3 xl yl zl, Vector3 xu yu zu) (Vector3 x y z) =
    inRange ((xl, yl, zl), (xu, yu, zu)) (x, y, z)
  index (Vector3 xl yl zl, Vector3 xu yu zu) (Vector3 x y z) =
    index ((xl, yl, zl), (xu, yu, zu)) (x, y, z)

fromTuple :: (Int, Int, Int) -> Vector3
fromTuple (x, y, z) =
  Vector3 x y z

zero :: Vector3
zero = Vector3 0 0 0

add :: Vector3 -> Vector3 -> Vector3
add (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) =
  Vector3 (x1 + x2) (y1 + y2) (z1 + z2)

scale :: Int -> Vector3 -> Vector3
scale a (Vector3 x y z) =
  Vector3 (a * x) (a * y) (a * z)

instance Semigroup Vector3 where
  (<>) = add

instance Monoid Vector3 where
  mempty = zero

expandBounds :: Vector3 -> (Vector3, Vector3) -> (Vector3, Vector3)
expandBounds (Vector3 x y z) (Vector3 xl yl zl, Vector3 xu yu zu) =
  let lower = Vector3 (min x xl) (min y yl) (min z zl)
      upper = Vector3 (max x xu) (max y yu) (max z zu)
   in (lower, upper)

getX :: Vector3 -> Int
getX (Vector3 v _ _) = v 

getY :: Vector3 -> Int
getY (Vector3 _ v _) = v 

getZ :: Vector3 -> Int
getZ (Vector3 _ _ v) = v 