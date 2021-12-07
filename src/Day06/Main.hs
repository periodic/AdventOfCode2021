module Main where

import qualified Data.Attoparsec.Text as P
import Exercise
import Numeric.LinearAlgebra hiding ((<>))
import qualified Data.Map as M
import Data.List
import Text.Printf ( printf )

-- Parsing
--------------------------------------------------

type Input = [Int]

fishParser :: P.Parser [Int]
fishParser =
  P.decimal `P.sepBy` P.char ','

inputParser :: P.Parser Input
inputParser =
  fishParser <* P.skipSpace <* P.endOfInput

-- Linear Algebra
--------------------------------------------------

-- | Simple matrix powers.
-- Does not use repeated squaring.
pow :: Matrix R -> Int -> Matrix R
pow m n
  | n == 0 = ident (rows m)
  | n == 1 = m
  | otherwise =
    m <> pow m (n -1)

-- | Convert a list of numbers to a vector of counts
-- This could be more efficient by accumulating counts, but this is good enough.
toCounts :: [Int] -> Vector R
toCounts = fromList . toFrequencies . M.fromListWith (+) . flip zip (repeat 1)
  where
    toFrequencies m =
      map (maybe 0 id . flip M.lookup m) [0..8]

-- Main
--------------------------------------------------

main :: IO ()
main = do
  input <- parseInput inputParser
  numFish1 <- runExercise "Part 1" (countFish 7 2 80) input
  printf "Number of fish: %d\n" numFish1
  numFish2 <- runExercise "Part 2" (countFish 7 2 256) input
  printf "Number of fish: %d\n" numFish2

-- numFish2 <- runExercise "Part 2" part2 input
-- printf "Losing Score: %d\n" score2

countFish :: Int -> Int -> Int -> Input -> Int
countFish gestation adolescence generations fish =
  let fishVector = toCounts fish
      generationMatrix = makeGenerationMatrix gestation adolescence
   in truncate . sumElements $ pow generationMatrix generations #> fishVector

makeGenerationMatrix :: Int -> Int -> Matrix R
makeGenerationMatrix gestation adolescence =
  let size = gestation + adolescence
      modifyNth n f xs =
        let (a, b : c) = splitAt n xs
         in a ++ f b : c
      emptyRow = replicate size 0
      row n = modifyNth n (+ 1) emptyRow
      basic = map row [1 .. (size - 1)] ++ [row 0]
      withGestation = modifyNth (gestation - 1) (modifyNth 0 (+ 1)) basic
   in fromLists withGestation