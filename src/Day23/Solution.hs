module Solution where

import qualified Data.Attoparsec.Text as P
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map
import Exercise
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

data Shrimp = Amber | Bronze | Copper | Desert deriving (Eq, Enum, Ord)

instance Show Shrimp where
  show Amber = "A"
  show Bronze = "B"
  show Copper = "C"
  show Desert = "D"

cost :: Shrimp -> Int
cost Amber = 1
cost Bronze = 10
cost Copper = 100
cost Desert = 1000

data Location
  = Hall Int
  | Room Shrimp Int
  deriving (Eq, Show, Ord)

data ShrimpHotel = ShrimpHotel
  { depth :: Int,
    locations :: Map Location Shrimp
  } deriving (Eq, Ord)

instance Show ShrimpHotel where
  show ShrimpHotel {depth, locations} =
    "#############\n"
      ++ "#"
      ++ concatMap (s . Hall) [1 .. 11]
      ++ "#\n"
      ++ concatMap (\d ->
        "###"
        ++ s (Room Amber d)
        ++ "#"
        ++ s (Room Bronze d)
        ++ "#"
        ++ s (Room Copper d)
        ++ "#"
        ++ s (Room Desert d)
        ++ "###\n"
      ) [1..depth]
      ++ "  #########"
    where
      s = maybe "." show . (`Map.lookup` locations)

emptyHotel :: Int -> ShrimpHotel
emptyHotel depth =
  ShrimpHotel depth Map.empty

hasShrimpAt :: ShrimpHotel -> Location -> Bool
hasShrimpAt ShrimpHotel {locations} = (`Map.member` locations)

getShrimpAt :: ShrimpHotel -> Location -> Maybe Shrimp
getShrimpAt ShrimpHotel {locations} = (`Map.lookup` locations)

moveShrimp :: Location -> Location -> ShrimpHotel -> ShrimpHotel
moveShrimp from to ShrimpHotel {locations, depth} =
  ShrimpHotel depth . Map.insert to (locations Map.! from) $ Map.delete from locations

isSolved :: ShrimpHotel -> Bool
isSolved hotel =
  and [getShrimpAt hotel (Room s n) == Just s | s <- [Amber .. Desert], n <- [1 .. depth hotel]]

hallForShrimp :: Shrimp -> Location
hallForShrimp Amber = Hall 3
hallForShrimp Bronze = Hall 5
hallForShrimp Copper = Hall 7
hallForShrimp Desert = Hall 9

stableHallPositions :: [Location]
stableHallPositions = [Hall 1, Hall 2, Hall 4, Hall 6, Hall 8, Hall 10, Hall 11]

distance :: Location -> Location -> Int
distance (Hall a) (Hall b) = abs (a - b)
distance (Room shrimp n) l = n + distance (hallForShrimp shrimp) l
distance hall (Room shrimp n) = distance hall (hallForShrimp shrimp) + n

path :: Location -> Location -> [Location]
path (Hall a) (Hall b) = if a > b then map Hall [a -1, a -2 .. b] else map Hall [a + 1, a + 2 .. b]
path (Room shrimp n) l = [Room shrimp r | r <- [n-1, n-2..1]] ++ hallForShrimp shrimp : path (hallForShrimp shrimp) l
path hall (Room shrimp n) = path hall (hallForShrimp shrimp) ++ map (Room shrimp) [1 .. n]

hasClearPath :: ShrimpHotel -> Location -> Location -> Bool
hasClearPath hotel from to =
  all (not . hasShrimpAt hotel) $ path from to

possibleMoves :: ShrimpHotel -> Shrimp -> Location -> [(Location, Int)]
possibleMoves hotel shrimp start@(Hall _) =
  let rooms = [Room shrimp n | n <- [1 .. depth hotel]]
      availableRooms = takeWhile (not . hasShrimpAt hotel) rooms
      occupiedRooms = filter (hasShrimpAt hotel) rooms
      end = head . reverse $ availableRooms
      outerRoom = Room shrimp 1
   in
     if not (hotel `hasClearPath` start $ outerRoom) -- no clear path
       || length availableRooms == 0 -- No space to move
       || any ((/= Just shrimp) . getShrimpAt hotel) occupiedRooms -- has a different shrimp
       then []
       else [(end, cost shrimp * distance start end)]
possibleMoves hotel shrimp start@(Room roomType n)
  | roomType == shrimp
    && all ((== Just shrimp) . (hotel `getShrimpAt`)) [Room roomType r | r <- [n..depth hotel]] =
    [] -- If it's in the right room and it's not blocking someone in then stay
  | any (hotel `hasShrimpAt`) [Room roomType r | r <- [1..n-1]] =
    [] -- Blocked in
  | otherwise =
    map (\end -> (end, cost shrimp * distance start end))
      . filter (\end -> hotel `hasClearPath` start $ end)
      $ stableHallPositions

possibleFutures :: ShrimpHotel -> [(Int, ShrimpHotel, String)]
possibleFutures hotel =
  concatMap (uncurry makeMoves) . Map.assocs . locations $ hotel
  where
    makeMoves :: Location -> Shrimp -> [(Int, ShrimpHotel, String)]
    makeMoves start shrimp =
      map (\(end, moveCost) -> (moveCost, moveShrimp start end hotel, describe shrimp start end)) $ possibleMoves hotel shrimp start
    describe shrimp start end =
      "Move " ++ show shrimp ++ " from " ++ show start ++ " to " ++ show end

minCostToSolution :: ShrimpHotel -> Int
minCostToSolution hotel =
  sum . map costToDestinationRoom . Map.assocs . locations $ hotel
  where
    costToDestinationRoom :: (Location, Shrimp) -> Int
    costToDestinationRoom (start, shrimp) =
      case start of
        (Room s n) ->
          if s == shrimp
            then 0
            else cost shrimp * distance start (Room shrimp 1)
        (Hall n) -> 
          cost shrimp * distance start (Room shrimp 1)

type QueueEntry = (Int, Int, ShrimpHotel, [String])
type Queue = Set QueueEntry
type Visited = Set ShrimpHotel

searchForSolution :: ShrimpHotel -> (Int, ShrimpHotel, [String])
searchForSolution hotel =
  let queue = Set.singleton (minCostToSolution hotel, 0, hotel, [])
  in runSearch queue (Set.empty)
  where
    runSearch :: Queue -> Visited -> (Int, ShrimpHotel, [String])
    runSearch queue visited =
      let (hotel, cost, log, newQueue, newVisited) = stepSearch queue visited
      in if isSolved hotel
           then (cost, hotel, log)
           else runSearch newQueue newVisited

stepSearch :: Queue -> Visited -> (ShrimpHotel, Int, [String], Queue, Visited)
stepSearch queue visited =
  case Set.minView queue of
    Nothing -> error "Ran out of things to search!"
    Just ((_, cost, hotel, log), queue') ->
      let futures = filter (\(_, h, _) -> not $ Set.member h visited) $ possibleFutures hotel
          entries = map (\(moveCost, newHotel, description) ->
            buildQueueEntry moveCost newHotel description cost log) futures
          newQueue = Set.fromList entries `Set.union` queue'
          newVisited = Set.insert hotel visited
      in if Set.member hotel visited
           then stepSearch queue' visited
           else (hotel, cost, log, newQueue, newVisited)

buildQueueEntry :: Int -> ShrimpHotel -> String -> Int -> [String] -> QueueEntry
buildQueueEntry moveCost hotel logEntry prevCost log =
  (estimate + prevCost + moveCost, prevCost + moveCost, hotel, logEntry : log)
  where
    estimate = minCostToSolution hotel

type Input = ShrimpHotel

parseShrimp :: P.Parser Shrimp
parseShrimp =
  P.choice
    [ Amber <$ "A",
      Bronze <$ "B",
      Copper <$ "C",
      Desert <$ "D"
    ]

parseHotel :: P.Parser ShrimpHotel
parseHotel = do
  "#############" *> P.endOfLine
  "#...........#" *> P.endOfLine
  "###"
  aRoom1 <- parseShrimp
  "#"
  bRoom1 <- parseShrimp
  "#"
  cRoom1 <- parseShrimp
  "#"
  dRoom1 <- parseShrimp
  "###" *> P.endOfLine
  "  #"
  aRoom2 <- parseShrimp
  "#"
  bRoom2 <- parseShrimp
  "#"
  cRoom2 <- parseShrimp
  "#"
  dRoom2 <- parseShrimp
  "#" *> P.endOfLine
  "  #########"
  return $
    ShrimpHotel 2 . Map.fromList $
      [ (Room Amber 1, aRoom1),
        (Room Amber 2, aRoom2),
        (Room Bronze 1, bRoom1),
        (Room Bronze 2, bRoom2),
        (Room Copper 1, cRoom1),
        (Room Copper 2, cRoom2),
        (Room Desert 1, dRoom1),
        (Room Desert 2, dRoom2)
      ]

expandHotel :: ShrimpHotel -> ShrimpHotel
expandHotel hotel =
  let ShrimpHotel {locations} = foldr (\r -> moveShrimp (Room r 2) (Room r 4)) hotel [Amber .. Desert]
  in ShrimpHotel {
    depth = 4,
    locations =
      Map.insert (Room Amber 2) Desert
      . Map.insert (Room Amber 3) Desert
      . Map.insert (Room Bronze 2) Copper
      . Map.insert (Room Bronze 3) Bronze
      . Map.insert (Room Copper 2) Bronze
      . Map.insert (Room Copper 3) Amber
      . Map.insert (Room Desert 2) Amber
      . Map.insert (Room Desert 3) Copper
      $ locations
  }


solution :: Solution Input
solution =
  Solution
    { solutionParser = parseHotel,
      solutionPart1 = (\(cost, _, _) -> cost) . searchForSolution,
      solutionPart2 = (\(cost, _, _) -> cost) . searchForSolution . expandHotel
    }

exampleHotel :: ShrimpHotel
exampleHotel =
  ShrimpHotel 2 . Map.fromList $
    [ (Room Amber 1, Bronze),
      (Room Amber 2, Amber),
      (Room Bronze 1, Copper),
      (Room Bronze 2, Desert),
      (Room Copper 1, Bronze),
      (Room Copper 2, Copper),
      (Room Desert 1, Desert),
      (Room Desert 2, Amber)
    ]

inputHotel :: ShrimpHotel
inputHotel =
  ShrimpHotel 2 . Map.fromList $
    [ (Room Amber 1, Desert),
      (Room Amber 2, Copper),
      (Room Bronze 1, Desert),
      (Room Bronze 2, Amber),
      (Room Copper 1, Bronze),
      (Room Copper 2, Bronze),
      (Room Desert 1, Amber),
      (Room Desert 2, Copper)
    ]