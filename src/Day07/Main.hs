module Main where

import qualified Data.Attoparsec.Text as P
import Exercise
import Text.Printf (printf)
import Data.List (sort, minimumBy)

-- Parsing
--------------------------------------------------

type Input = [Int]

inputParser :: P.Parser Input
inputParser =
  (P.decimal `P.sepBy` P.char ',') <* P.skipSpace <* P.endOfInput

-- Main
--------------------------------------------------

main :: IO ()
main = do
  input <- parseInput inputParser
  answer1 <- runExercise "Part 1" part1 input
  printf "Part 1: %d\n" answer1
  answer2 <- runExercise "Part 2" part2 input
  printf "Part 2: %d\n" answer2

median :: [Int] -> Int
median xs =
  let len = length xs
      (left, right) = splitAt (len `div` 2) $ sort xs
  in head right

fuelCost1 :: Int -> [Int] -> Int
fuelCost1 target =
  sum . map (abs . (target -))

part1 :: Input -> Int
part1 crabs =
  (`fuelCost1` crabs) (median crabs)

findMin :: (Int -> Int) -> (Int, Int) -> Int
findMin f (minBound, maxBound) =
  minimumBy (\a b -> f a `compare` f b) [minBound .. maxBound]

fuelCost2 :: Int -> Int -> Int
fuelCost2 target =
  (\x -> (x^2 + abs x) `div` 2) . (target -)

part2 :: Input -> Int
part2 crabs =
  let
    (minPos, macPos) = (minimum crabs, maximum crabs)
    fuelCostAll x = sum . map (fuelCost2 x) $ crabs
    minLocation = findMin fuelCostAll (minPos, macPos)
  in
    fuelCostAll minLocation
