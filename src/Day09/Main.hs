module Main where

import qualified Data.Attoparsec.Text as P
import Exercise
import Text.Printf (printf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Control.Monad.State
import Control.Monad.Reader
import Data.List (sort)

-- Parsing
--------------------------------------------------

type Coord = (Int, Int)
type Input = Map Coord Int

indexed = zip [1..]

inputParser :: P.Parser Input
inputParser =
  Map.fromList . flatten <$> allData
  where
    flatten :: [(Int, [(Int, Int)])] -> [(Coord, Int)]
    flatten rows = do
      (y, row) <- rows
      (x, val) <- row
      return ((x, y), val)
    allData :: P.Parser [(Int, [(Int, Int)])]
    allData =
      indexed <$> row `P.sepBy` P.endOfLine <* P.skipSpace <* P.endOfInput
    row :: P.Parser [(Int, Int)]
    row =
      indexed . map (read . (:[])) <$> P.many1 P.digit 

-- Main
--------------------------------------------------

main :: IO ()
main = do
  input <- parseInput inputParser
  answer1 <- runExercise "Part 1" part1 input
  printf "Part 1: %d\n" answer1
  answer2 <- runExercise "Part 2" part2 input
  printf "Part 2: %d\n" answer2

neighbors :: Coord -> [Coord]
neighbors (x,y) =
  [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

riskLevel :: Input -> Coord -> Maybe Int
riskLevel heightMap coord =
  let
    val = heightMap Map.! coord
    neighborVals = mapMaybe (`Map.lookup` heightMap) . neighbors $ coord
    isLowPoint = all (> val) neighborVals
  in
    if isLowPoint
      then Just $ 1 + val
      else Nothing

part1 :: Input -> Int
part1 heightMap =
  sum . mapMaybe (riskLevel heightMap) . Map.keys $ heightMap

newtype Basin a = Basin {
  runBasins :: ReaderT Input (State (Set Coord)) a
} deriving (Functor, Applicative, Monad, MonadReader Input, MonadState (Set Coord))

fillBasin :: Coord -> Basin Int
fillBasin coord = do
  visited <- isVisited coord
  val <- getValue coord
  if visited || val == 9
    then return 0
    else do
      markVisited coord
      (+1) . sum <$> mapM fillBasin (neighbors coord)
  where
    isVisited =
      gets . Set.member 
    markVisited =
      modify . Set.insert
    getValue =
      asks . Map.findWithDefault 9 

allBasins :: Basin [Int]
allBasins = do
  coords <- Map.keys <$> ask
  filter (>0) <$> mapM fillBasin coords

part2 :: Input -> Int
part2 input =
  let
    run = flip evalState Set.empty . flip runReaderT input . runBasins
  in 
    product . take 3 . reverse . sort $ run allBasins
  