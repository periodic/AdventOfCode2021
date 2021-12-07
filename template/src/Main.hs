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
  past1 <- runExercise "Part 1" part1 input
  printf "Part 1: %d\n" part1
  part2 <- runExercise "Part 2" part2 input
  printf "Part 2: %d\n" part2

part1 :: Input -> ()
part1 =
  undefined

part2 :: Input -> ()
part2 =
  undefined