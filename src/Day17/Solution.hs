module Solution where

import qualified Data.Attoparsec.Text as P
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Exercise

data Input = Input
  { minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int
  }
  deriving (Show)

inputParser :: P.Parser Input
inputParser = do
  "target area: x="
  minX <- P.signed P.decimal
  ".."
  maxX <- P.signed P.decimal
  ", y="
  minY <- P.signed P.decimal
  ".."
  maxY <- P.signed P.decimal
  return $ Input {minX, maxX, minY, maxY}

highestAltitude :: Input -> Int
highestAltitude Input {minY} =
  let vy = negate minY
   in vy * (vy - 1) `div` 2

indexed = zip [1 ..]

-- | Creates a map of VY values that land in the target on various iteration counts.
possibleVY :: Input -> Map Int [Int]
possibleVY Input {minY, maxY} =
  Map.fromListWith (++) . concatMap targetYs $ [minY .. (negate minY - 1)]
  where
    ys vy0 =
      map (\(i, _) -> (i, vy0))
        . dropWhile (\(i, (y, vy)) -> y > maxY)
        . indexed
        . takeWhile (\(y, vy) -> y >= minY)
        . iterate (\(y, vy) -> (y + vy, vy - 1))
        $ (0, vy0)
    targetYs = map (\(i, vy) -> (i, [vy])) . ys

possibleSolutions :: Input -> Map Int [Int] -> Set (Int, Int)
possibleSolutions Input {minX, maxX} vyMap =
  Set.fromList $ concatMap vxSolutions [1 .. maxX]
  where
    (maxI, _) = Map.findMax vyMap
    xsIndexed vx =
      map (\(i, _) -> (i, vx))
        . dropWhile (\(i, (x, vx)) -> x < minX)
        . takeWhile (\(i, (x, vx)) -> x <= maxX && i <= maxI)
        . indexed
        . iterate (\(x, vx) -> (x + vx, if vx > 0 then vx - 1 else 0))
        $ (0, vx)
    vxSolutions vx =
      concatMap
        ( \(i, vx) ->
            map (\vy -> (vx, vy)) . Map.findWithDefault [] i $ vyMap
        )
        . xsIndexed
        $ vx

allSolutions :: Input -> Set (Int, Int)
allSolutions input =
  possibleSolutions input (possibleVY input)

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = highestAltitude,
      solutionPart2 = Set.size . allSolutions
    }

exampleInput :: Input
exampleInput =
  Input
    { minX = 20,
      maxX = 30,
      minY = -10,
      maxY = -5
    }

exampleSolutions :: Set (Int, Int)
exampleSolutions =
  Set.fromList
    [ (23, -10),
      (25, -9),
      (27, -5),
      (29, -6),
      (22, -6),
      (21, -7),
      (9, 0),
      (27, -7),
      (24, -5),
      (25, -7),
      (26, -6),
      (25, -5),
      (6, 8),
      (11, -2),
      (20, -5),
      (29, -10),
      (6, 3),
      (28, -7),
      (8, 0),
      (30, -6),
      (29, -8),
      (20, -10),
      (6, 7),
      (6, 4),
      (6, 1),
      (14, -4),
      (21, -6),
      (26, -10),
      (7, -1),
      (7, 7),
      (8, -1),
      (21, -9),
      (6, 2),
      (20, -7),
      (30, -10),
      (14, -3),
      (20, -8),
      (13, -2),
      (7, 3),
      (28, -8),
      (29, -9),
      (15, -3),
      (22, -5),
      (26, -8),
      (25, -8),
      (25, -6),
      (15, -4),
      (9, -2),
      (15, -2),
      (12, -2),
      (28, -9),
      (12, -3),
      (24, -6),
      (23, -7),
      (25, -10),
      (7, 8),
      (11, -3),
      (26, -7),
      (7, 1),
      (23, -9),
      (6, 0),
      (22, -10),
      (27, -6),
      (8, 1),
      (22, -8),
      (13, -4),
      (7, 6),
      (28, -6),
      (11, -4),
      (12, -4),
      (26, -9),
      (7, 4),
      (24, -10),
      (23, -8),
      (30, -8),
      (7, 0),
      (9, -1),
      (10, -1),
      (26, -5),
      (22, -9),
      (6, 5),
      (7, 5),
      (23, -6),
      (28, -10),
      (10, -2),
      (11, -1),
      (20, -9),
      (14, -2),
      (29, -7),
      (13, -3),
      (23, -5),
      (24, -8),
      (27, -9),
      (30, -7),
      (28, -5),
      (21, -10),
      (7, 9),
      (6, 6),
      (21, -5),
      (27, -10),
      (7, 2),
      (30, -9),
      (21, -8),
      (22, -7),
      (24, -9),
      (20, -6),
      (6, 9),
      (29, -5),
      (8, -2),
      (27, -8),
      (30, -5),
      (24, -7)
    ]