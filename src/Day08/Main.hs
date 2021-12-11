module Main where

import qualified Data.Attoparsec.Text as P
import Exercise
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf (printf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find)
import Data.Maybe (fromMaybe)

-- Parsing
--------------------------------------------------

type Digit = Set Char

data Display = Display {
  displayReadings :: [Digit],
  displayValues :: [Digit]
} deriving (Show)

type Input = [Display]

digit :: P.Parser Digit
digit =
  Set.fromList <$> P.many1 P.letter

display :: P.Parser Display
display =
  Display <$> digits <* separator <*> digits
  where
    whitespace = P.takeWhile1 P.isHorizontalSpace
    digits = digit `P.sepBy1` whitespace
    separator = whitespace *> P.char '|' *> whitespace 

inputParser :: P.Parser Input
inputParser =
  (display `P.sepBy1` P.skipSpace) <* P.skipSpace <* P.endOfInput
  

-- Main
--------------------------------------------------

main :: IO ()
main = do
  input <- parseInput inputParser
  print . length $ input
  answer1 <- runExercise "Part 1" part1 input
  printf "Part 1: %d\n" answer1
  answer2 <- runExercise "Part 2" part2 input
  printf "Part 2: %d\n" answer2

part1 :: Input -> Int
part1 =
  foldr ( (+) . length . filter ((`elem` [2,3,4,7]) . Set.size) . displayValues) 0
  
type Mapping = Map Char Char

applyMapping :: Mapping -> Digit -> Digit
applyMapping m =
  Set.map (m Map.!)

zero = Set.fromList "abcefg"
one = Set.fromList "cf"
two = Set.fromList "acdeg"
three = Set.fromList "acdfg"
four = Set.fromList "bcdf"
five = Set.fromList "abdfg"
six = Set.fromList "abdefg"
seven = Set.fromList "acf"
eight = Set.fromList "abcdefg"
nine = Set.fromList "abcdfg"

allDigits = Set.fromList [zero, one, two, three, four, five, six, seven, eight, nine]

toDigit :: Digit -> Int
toDigit d 
  | d == zero = 0
  | d == one = 1
  | d == two = 2
  | d == three = 3
  | d == four = 4
  | d == five = 5
  | d == six = 6
  | d == seven = 7
  | d == eight = 8
  | d == nine = 9
  | otherwise = error "toDigit: invalid digit"

solveMapping :: Display -> Mapping
solveMapping Display { displayReadings = r } =
  let
    twoCells = head . filter ((== 2) . Set.size) $ r
    threeCells = head . filter ((== 3) . Set.size) $ r
    fourCells = head . filter ((== 4) . Set.size) $ r
    fiveCells = head . filter ((== 5) . Set.size) $ r
    sixCells = head . filter ((== 6) . Set.size) $ r
    sevenCells = head . filter ((== 7) . Set.size) $ r
    aCell = threeCells `Set.difference` twoCells -- Top of 7 minus 1
    bCell = fourCells `Set.difference` twoCells -- 4 minus 1
    cCell = twoCells -- Two parts of 1
    dCell = fourCells `Set.difference` twoCells -- 4 minus 1
    eCell = sevenCells `Set.difference` threeCells `Set.difference` fourCells -- All cells minus 4 and 7
    fCell = twoCells -- Two parts of 1
    gCell = eCell -- Same as above
    options :: [[(Char, Char)]]
    options = do
      a <- Set.toList aCell
      b <- Set.toList bCell
      c <- Set.toList cCell
      d <- Set.toList dCell
      e <- Set.toList eCell
      f <- Set.toList fCell
      g <- Set.toList gCell
      [[(a, 'a'), (b, 'b'), (c, 'c'), (d, 'd'), (e, 'e'), (f, 'f'), (g, 'g')]]
    possibleMaps :: [Mapping]
    possibleMaps = 
      map Map.fromList options
    isGoodMap :: Map Char Char -> Bool
    isGoodMap candidateMap =
      let
        isCompleteMap = Map.size candidateMap == 7
        mapsAllDigits = (== allDigits) . Set.fromList . map (applyMapping candidateMap) $ r
      in 
        isCompleteMap && mapsAllDigits
  in
    fromMaybe (error $ "No mapping found: " ++ show r) $ find isGoodMap possibleMaps

displayValue :: Display -> Int
displayValue display =
  let 
    mapping = solveMapping display
    [a, b, c, d] = map (toDigit . applyMapping mapping) . displayValues $ display
  in
    a * 1000 + b * 100 + c * 10 + d

part2 :: Input -> Int
part2 =
  sum . map displayValue