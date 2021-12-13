module Main where

import Exercise ( parseInput, runExercise )
import Solution ( part1, part2, inputParser )
import Text.Printf ( printf )

main :: IO ()
main = do
  input <- parseInput inputParser
  part1 <- runExercise "Part 1" part1 input
  printf "Num. Dots: %d\n" part1
  part2 <- runExercise "Part 2" part2 input
  putStrLn part2