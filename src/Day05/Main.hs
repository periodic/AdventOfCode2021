module Main where

import qualified Data.Attoparsec.Text as P
import qualified Data.Map as M
import Text.Printf (printf)
import Exercise

data Coordinate = C { x :: Int, y :: Int } deriving (Eq, Ord)

instance Show Coordinate where
  show (C x y) = printf "(%d,%d)" x y

data Vent = Vent Coordinate Coordinate deriving (Show, Eq)

-- Parsing
--------------------------------------------------

coordinateParser :: P.Parser Coordinate
coordinateParser = do
  x <- P.decimal
  _ <- P.char ','
  y <- P.decimal
  return $ C x y

ventParser :: P.Parser Vent
ventParser = do
  c1 <- coordinateParser
  _ <- P.skipSpace *> P.string "->" *> P.skipSpace
  c2 <- coordinateParser
  return $ Vent c1 c2

inputParser :: P.Parser [Vent]
inputParser = (ventParser `P.sepBy` P.skipSpace) <* P.skipSpace <* P.endOfInput

-- VentMap
--------------------------------------------------

newtype VentMap = VentMap { pointData :: M.Map Coordinate Int } deriving (Show, Eq)

emptyVentMap :: VentMap
emptyVentMap = VentMap M.empty

ventCoords :: Vent -> [Coordinate]
ventCoords (Vent (C x1 y1) (C x2 y2)) =
  let
    xdir = signum $ x2 - x1
    ydir = signum $ y2 - y1
    xs = [x1,(x1 + xdir)..x2]
    ys = [y1,(y1 + ydir)..y2]
  in
    zipWith C xs ys


addVent :: Vent -> VentMap -> VentMap
addVent v@(Vent c1 c2) (VentMap m) =
  let
    cs = ventCoords v
  in
    VentMap $ foldr (\c -> M.insertWith (+) c 1) m cs

countPoints :: (Int -> Bool) -> VentMap -> Int
countPoints f =
  M.size . M.filter f . pointData

-- Part 1
--------------------------------------------------

filterOutDiagonals :: [Vent] -> [Vent]
filterOutDiagonals = 
  filter (\(Vent (C x1 y1) (C x2 y2)) -> x1 == x2 || y1 == y2)

part1 :: [Vent] -> Int
part1 =
    countPoints (>= 2) . foldr addVent emptyVentMap . filterOutDiagonals

-- Part 1
--------------------------------------------------

part2 :: [Vent] -> Int
part2 =
    countPoints (>= 2) . foldr addVent emptyVentMap

-- Main
--------------------------------------------------

main = do
  vents <- parseInput inputParser
  printf "Number of vents: %d\n" (length vents)
  part1 <- runExercise "Part 1" part1 vents
  printf "Number of points with overlapping vents: %d\n" part1
  part2 <- runExercise "Part 2" part2 vents
  printf "Number of points with overlapping vents: %d\n" part2


