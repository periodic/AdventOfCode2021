module Main where

import Control.Applicative
import Control.Monad (guard)
import qualified Data.Attoparsec.Text as P
import qualified Data.Either as Either
import Data.Ix
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Exercise
import Text.Printf

-- Bingo
--------------------------------------------------

type Coord = (Int, Int)

newtype BingoBoard = BingoBoard
  {bingoBoard :: M.Map Int Coord}
  deriving (Show)

-- Parsing
--------------------------------------------------

type Input = ([Int], [BingoBoard])

picks =
  P.decimal `P.sepBy` P.char ','

board =
  listToBoard
    <$> row `P.sepBy1` P.endOfLine
  where
    row = horizontalSpace *> P.decimal `P.sepBy1` horizontalSpace
    horizontalSpace = P.skipWhile P.isHorizontalSpace
    listToBoard =
      BingoBoard
        . M.fromList
        . concatMap (\(y, row) -> map (\(x, val) -> (val, (x, y))) . indexed $ row)
        . indexed

indexed = zip [1 ..]

parser =
  (,)
    <$> picks
    <* P.endOfLine
    <* P.endOfLine
    <*> (board `P.sepBy` (P.endOfLine *> P.endOfLine))

-- Playing
--------------------------------------------------

data IncompleteBoard = IncompleteBoard
  {playingBoard :: BingoBoard, playsMarked :: S.Set Coord}
  deriving (Show)

data WinningBoard = WinningBoard
  {winningBoard :: BingoBoard, winningMarked :: S.Set Coord, score :: Int}
  deriving (Show)

placePick :: Int -> IncompleteBoard -> Either WinningBoard IncompleteBoard
placePick pick playing@IncompleteBoard {playingBoard, playsMarked} =
  case M.lookup pick $ bingoBoard playingBoard of
    Nothing -> Right playing
    Just location ->
      let playsMarked' = S.insert location playsMarked
          playing' = playing {playsMarked = playsMarked'}
       in case winningScore playing' of
            Just score -> Left $ WinningBoard playingBoard playsMarked' score
            Nothing -> Right $ IncompleteBoard playingBoard playsMarked'

sumOfUnpicked :: IncompleteBoard -> Int
sumOfUnpicked IncompleteBoard {playingBoard, playsMarked} =
  sum
    . map fst
    . filter (not . flip S.member playsMarked . snd)
    . M.assocs
    . bingoBoard
    $ playingBoard

winningScore :: IncompleteBoard -> Maybe Int
winningScore incomplete@IncompleteBoard {playingBoard, playsMarked} =
  let rowIndexes = map (\y -> map (,y) [1 .. 5]) [1 .. 5]
      colIndexes = map (\x -> map (x,) [1 .. 5]) [1 .. 5]
      winningScore indexes = do
        guard $ all (`S.member` playsMarked) indexes
        return $ sumOfUnpicked incomplete
   in foldr ((<|>) . winningScore) Nothing (rowIndexes ++ colIndexes)

-- Main
--------------------------------------------------

main :: IO ()
main = do
  input <- parseInput parser
  WinningBoard {score = score1} <- runExercise "Part 1" part1 input
  printf "Winning Score: %d\n" score1
  WinningBoard {score = score2} <- runExercise "Part 2" part2 input
  printf "Losing Score: %d\n" score2

startGame :: BingoBoard -> IncompleteBoard
startGame board = IncompleteBoard board S.empty

runGame :: [Int] -> [IncompleteBoard] -> WinningBoard
runGame [] boards =
  error "Whoops, no more picks!"
runGame _ [] =
  error "Whoops, there must be at least one board!"
runGame (p : ps) boards =
  let boards' = map (placePick p) boards
   in case List.find Either.isLeft boards' of
        Just (Left winner) -> winner {score = score winner * p}
        Nothing -> runGame ps . Either.rights $ boards'

part1 :: Input -> WinningBoard
part1 (picks, boards) =
  runGame picks . map startGame $ boards

playUntilWin :: [Int] -> IncompleteBoard -> WinningBoard
playUntilWin picks board =
  let advanceBoard (p : ps) board = case placePick p board of
        Left win -> win {score = score win * p}
        Right board' -> advanceBoard ps board'
   in advanceBoard picks board

runGame2 :: [Int] -> [IncompleteBoard] -> WinningBoard
runGame2 [] _ = error "Whoops, ran out of picks!"
runGame2 ps [board] = playUntilWin ps board
runGame2 _ [] = error "Whoops, no one is left!"
runGame2 (p : ps) boards =
  runGame2 ps
    . Either.rights
    . map (placePick p)
    $ boards

part2 :: Input -> WinningBoard
part2 (picks, boards) =
  runGame2 picks . map startGame $ boards