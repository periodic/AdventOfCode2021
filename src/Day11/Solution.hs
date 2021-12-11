module Solution where

import qualified Data.Attoparsec.Text as P
import Data.Map (Map)
import qualified Data.Map as Map
import Exercise
import Dumbos

type Input = Map Coord Int

indexed = zip [1..]

inputParser =
  Map.fromList . toAssocs . indexed <$>
    (row `P.sepBy` P.endOfLine) <* P.skipSpace <* P.endOfInput
  where
    row :: P.Parser [(Int, Int)]
    row =
      indexed <$> P.many1 (read . (:[]) <$> P.digit)
    toAssocs =
      concatMap (\(y, xs) -> map (\(x, v) -> ((x, y), v)) xs)


part1 :: Input -> Int
part1 =
  fst . runCyclesFromMap 100

part2 :: Input -> Int
part2 =
  fst . runFromMap runUntilAllFlash

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = part1,
      solutionPart2 = part2
    }