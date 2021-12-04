module Main where

import Common (loadAndParseInput)
import qualified Data.Attoparsec.Text as P
import Text.Printf (printf)
import Data.Text (Text, concat, intercalate)
import Data.Vector (Vector)
import qualified Data.Vector as V

newtype Reading = Reading { readingValues :: Vector Int } deriving (Show)

fromBinary :: Reading -> Int
fromBinary =
  V.foldr (\(i, b) acc -> if b > 0 then 2 ^ i + acc else acc) 0 . V.indexed . V.reverse . readingValues

invert :: Reading -> Reading
invert = Reading . V.map (1 -) . readingValues


parseReading :: P.Parser Reading
parseReading =
  Reading . V.fromList . map (read . (:[])) <$> P.many1 P.digit

parser :: P.Parser [Reading]
parser = parseReading `P.sepBy` P.endOfLine

main :: IO ()
main = do
  input <- loadAndParseInput parser
  printf "Part 1: %d\n" $ part1 input
  printf "Part 2: %d\n" $ part2 input

mostCommon :: [Reading] -> Reading
mostCommon readings =
  let
    totalReadings = length readings
    sumReadings = foldr (V.zipWith (+) . readingValues) (V.replicate totalReadings 0) $ readings
    mostCommon = V.map (\n -> round (fromIntegral n / fromIntegral totalReadings)) sumReadings
  in
    Reading mostCommon

part1 :: [Reading] -> Int
part1 readings =
  let
    gammaReading = mostCommon readings
    gammaRate = fromBinary gammaReading
    epsilonRate = fromBinary . invert $ gammaReading
  in
    gammaRate * epsilonRate

part2 :: [Reading] -> Int
part2 readings =
  let o2Reading = filterReadings (>= 0.5) readings
      co2Reading = filterReadings (< 0.5) readings
      o2Rate = fromBinary o2Reading
      co2Rate = fromBinary co2Reading
  in
    o2Rate * co2Rate

filterReadings :: (Float -> Bool) -> [Reading] -> Reading
filterReadings = 
  filterFromIndex 0
  where
    filterFromIndex :: Int -> (Float -> Bool) -> [Reading] -> Reading
    filterFromIndex _ _ [] = error "Filtering eliminated all readings."
    filterFromIndex _ _ [reading] = reading
    filterFromIndex i pred readings =
      let
        numReadings = length readings
        sumOfIndex = sum . map ((V.! i) . readingValues) $ readings
        freqOfOnes = fromIntegral sumOfIndex / fromIntegral numReadings
        desiredBit = if pred freqOfOnes then 1 else 0
      in
        filterFromIndex (i + 1) pred $ filter (\r -> (V.! i) (readingValues r) == desiredBit) readings

testInput :: Text
testInput = Data.Text.intercalate "\n"
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]
