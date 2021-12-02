module Main where

import Common (loadAndParseInput)
import qualified Data.Attoparsec.Text as P
import Linear.V2
import Text.Printf (printf)

data Command
  = Up Int
  | Down Int
  | Forward Int

command :: P.Parser Command
command =
  let forward = Forward <$> (P.string "forward" *> P.skipSpace *> P.decimal)
      up = Up <$> (P.string "up" *> P.skipSpace *> P.decimal)
      down = Down <$> (P.string "down" *> P.skipSpace *> P.decimal)
   in P.choice [forward, up, down]

commandParser :: P.Parser [Command]
commandParser =
  command `P.sepBy` P.endOfLine

main :: IO ()
main = do
  commands <- loadAndParseInput commandParser
  printf "Part 1: %d\n" $ part1 commands
  printf "Part 2: %d\n" $ part2 commands

vectorToOutput :: V2 Int -> Int
vectorToOutput (V2 x y) = x * y

part1 :: [Command] -> Int
part1 =
  vectorToOutput . sum . map commandToVector
  where
    commandToVector = \case
      Up n -> V2 0 (negate n)
      Down n -> V2 0 n
      Forward n -> V2 n 0

part2 :: [Command] -> Int
part2 =
  vectorToOutput . fst . foldl runCommand (V2 0 0, 0)
  where
    runCommand (pos@(V2 x y), aim) = \case
      Up n -> (pos, aim - n)
      Down n -> (pos, aim + n)
      Forward n -> (V2 (x + n) (y + n * aim), aim)