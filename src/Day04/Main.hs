module Main where

import qualified Data.Array as A
import qualified Data.Attoparsec.Text as P
import qualified Data.Set as S
import Exercise
import Data.Maybe
import Control.Monad (guard)
import Control.Applicative
import Text.Printf
import Data.Ix

-- Bingo
--------------------------------------------------

type Coord = (Int, Int)

data BingoBoard = BingoBoard
  { bingoBoard :: A.Array Coord Int,
    bingoNumbers :: S.Set Coord
  }
  deriving (Show)

bingoBounds = ((1, 1), (5, 5))

placePick :: Int -> BingoBoard -> BingoBoard
placePick n board = 
  let
    foundLocation = listToMaybe . map fst . filter ((== n) . snd) $ A.assocs (bingoBoard board)
  in case foundLocation of
    Nothing -> board
    Just location ->
      board { bingoNumbers = S.insert location (bingoNumbers board) }

sumOfUnpicked :: BingoBoard -> Int
sumOfUnpicked board =
  sum
  . map (\index -> bingoBoard board A.! index)
  . filter (not . flip S.member (bingoNumbers board)) $ range bingoBounds

winningScore :: BingoBoard -> Maybe Int
winningScore board =
  let
    rowIndexes = map (\y -> map (, y) [1..5]) [1..5]
    colIndexes = map (\x -> map (x, ) [1..5]) [1..5]
    -- diagIndexes = map (\i -> (i, i)) [1..5]
    -- inverseDiagIndexes = map (\i -> (i, 6 - i)) [1..5]
    winningScore indexes = do
      guard $ all (\i -> S.member i (bingoNumbers board)) indexes
      return $ sumOfUnpicked board
  in
    foldr
      ((<|>) . winningScore)
      Nothing
      (rowIndexes
       ++ colIndexes
        -- ++ [diagIndexes, inverseDiagIndexes]
        )

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
      flip BingoBoard S.empty
        . A.array bingoBounds
        . concatMap (\(y, row) -> map (\(x, val) -> ((x, y), val)) . indexed $ row)
        . indexed

indexed = zip [1 ..]

parser =
  (,)
    <$> picks
    <* P.endOfLine
    <* P.endOfLine
    <*> (board `P.sepBy` (P.endOfLine *> P.endOfLine))

-- Main
--------------------------------------------------

main :: IO ()
main = do
  input <- parseInput parser
  (score, board) <- runExercise "Part 1" part1 input
  printf "Winning Score: %d\n" score
  printf "Winning Board: %s\n" $ show board
  (score2, board2) <- runExercise "Part 2" part2 input
  printf "Losing Score: %d\n" score2
  printf "Losing Board: %s\n" $ show board2

processPick :: Int -> BingoBoard -> Either (Int, BingoBoard) BingoBoard
processPick n board =
  let
    board' = placePick n board
  in case winningScore board' of
      Just score -> Left (score * n, board')
      Nothing -> Right board'

runGame :: [Int] -> [BingoBoard] -> Either (Int, BingoBoard) [BingoBoard]
runGame [] boards =
  error "Whoops, no one wins!"
runGame (p:ps) boards = do
  boards' <- mapM (processPick p) boards
  runGame ps boards'

part1 :: Input -> (Int, BingoBoard)
part1 (picks, boards) =
  case runGame picks boards of
    Left win -> win
    Right _ -> error "Whoops, no one wins!"

playUntilWin :: [Int] -> BingoBoard -> (Int, BingoBoard)
playUntilWin picks board =
    let advanceBoard (p:ps) board = case processPick p board of
          Left win -> win
          Right board' -> advanceBoard ps board'
    in advanceBoard picks board

runGame2 :: [Int] -> [BingoBoard] -> (Int, BingoBoard)
runGame2 [] _ = error "Whoops, ran out of picks!"
runGame2 ps [board] = playUntilWin ps board
runGame2 _ [] = error "Whoops, no one is left!"
runGame2 (p:ps) boards =
  runGame2 ps
  . filter (isNothing . winningScore)
  . map (placePick p) $ boards
  

part2 :: Input -> (Int, BingoBoard)
part2 (picks, boards) =
  runGame2 picks boards