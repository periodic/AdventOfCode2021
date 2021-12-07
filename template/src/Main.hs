module Main where

import qualified Data.Attoparsec.Text as P
import Exercise
import Text.Printf (printf)

-- Parsing
--------------------------------------------------

type Input = Void

inputParser :: P.Parser Input
inputParser =
  undefined

-- Main
--------------------------------------------------

main :: IO ()
main = do
  input <- parseInput inputParser
  answer1 <- runExercise "Part 1" part1 input
  printf "Part 1: %d\n" answer1
  answer2 <- runExercise "Part 2" part2 input
  printf "Part 2: %d\n" answer2

part1 :: Input -> Int
part1 =
  undefined

part2 :: Input -> Int
part2 =
  undefined