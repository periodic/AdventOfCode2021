module Main where

import qualified Data.Attoparsec.Text as P
import Common (loadAndParseInput)
import Text.Printf ( printf )

parser :: P.Parser [Int]
parser =
  P.sepBy P.decimal P.endOfLine

main :: IO ()
main = do
  depthReadings <- loadAndParseInput parser
  printf "Number of increasing readings: %d\n" $ part1 depthReadings
  printf "Number of increasing windows: %d\n" $ part2 depthReadings


part1 :: [Int] -> Int
part1 depthReadings =
  length . filter (uncurry (<)) . zip depthReadings . tail $ depthReadings
  
toWindows :: [Int] -> [Int]
toWindows readings =
  map (\(a, b, c) -> a + b + c) .
  zip3 readings (tail readings) . tail . tail $ readings

part2 :: [Int] -> Int
part2 = part1 . toWindows