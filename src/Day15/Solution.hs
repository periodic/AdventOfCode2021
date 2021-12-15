module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise
import ParserHelpers (parseGrid)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as Queue
import qualified Data.Set as Set
import qualified Data.Ix as Ix
import GHC.Base (undefined)

type Input = Map (Int, Int) Int

inputParser :: P.Parser Input
inputParser =
  Map.fromList <$> parseGrid row
  where
    row = map (read . (:[])) <$> P.many1 P.digit

neighbors :: Input -> (Int, Int) -> [(Int, Int)]
neighbors input (x, y) =
  filter (`Map.member` input) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

data Node = Node {
  nodePos :: (Int, Int),
  nodePath :: [(Int, Int)],
  nodeCost :: Int,
  nodeEstimate :: Int
} deriving (Show)

instance Eq Node where
  (Node p1 _ _ _) == (Node p2 _ _ _) = p1 == p2

instance Ord Node where
  compare a b = compare (nodeEstimate a) (nodeEstimate b)

findMinPath :: Input -> Int
findMinPath input =
  let
    start = fst . Map.findMin $ input
    queue = Queue.fromList . map (posToNode [] 0) . neighbors input $ start
  in
    nodeCost $ findMinPath' (Set.singleton start) queue
  where
    end = fst . Map.findMax $ input
    posToNode prevPath prevCost pos =
      let
        cost = prevCost + Map.findWithDefault 1 pos input
        estimate = distance pos end + cost
        path = pos : prevPath
      in
        Node pos path cost estimate
    findMinPath' visited queue =
      case Queue.minView queue of
        Nothing -> error "Queue is empty, no path found."
        Just (node@(Node pos path cost estimate), queue') ->
          if Set.member pos visited
            then findMinPath' visited queue'
            else
              if pos == end
                then node
                else
                  let
                    nextCoords = map (posToNode path cost) . neighbors input $ pos
                    visited' = Set.insert pos visited
                  in
                    findMinPath' visited' $ foldr Queue.insert queue' nextCoords


expandMap :: Input -> Input
expandMap input =
  foldr (Map.union . expandTo) Map.empty $ Ix.range ((0,0), (4,4))
  where
    (width, height) = fst . Map.findMax $ input
    expandTo (dx, dy) =
      let
        mapCoord (x, y) = (x + dx * width, y + dy * height)
        mapVal v = ((v - 1 + dx + dy) `mod` 9) + 1
      in
        Map.fromList . map (\(coord, v) -> (mapCoord coord, mapVal v)) . Map.toList $ input

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = findMinPath,
      solutionPart2 = findMinPath . expandMap
    }