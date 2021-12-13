module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Data.List

type Dot = (Int, Int)

data Instruction
  = Horizontal Int
  | Vertical Int
  deriving (Show, Eq)

data Input = Input
  { inputDots :: Set Dot,
    inputInstructions :: [Instruction]
  }
  deriving (Show, Eq)

dotParser :: P.Parser Dot
dotParser = 
  (,) <$> P.decimal <* P.char ',' <*> P.decimal

instructionParser :: P.Parser Instruction
instructionParser = do
  _ <- P.string "fold along "
  dir <- P.char 'x' <|> P.char 'y'
  P.char '='
  value <- P.decimal
  return $ case dir of
    'x' -> Horizontal value
    'y' -> Vertical value

inputParser :: P.Parser Input
inputParser = do
  dots <- Set.fromList <$> dotParser `P.sepBy` P.endOfLine
  P.skipSpace
  instructions <- instructionParser `P.sepBy` P.endOfLine
  P.skipSpace
  P.endOfInput
  return $ Input dots instructions

foldUp :: Int -> Set Dot -> Set Dot
foldUp along =
  Set.map foldDotUp 
  where
    foldDotUp (x, y)
      | y > along = (x, 2*along - y)
      | otherwise = (x, y)

foldLeft :: Int -> Set Dot -> Set Dot
foldLeft along =
  Set.map foldDotUp 
  where
    foldDotUp (x, y)
      | x > along = (2*along - x, y) 
      | otherwise = (x, y)

foldPaper :: Instruction -> Set Dot -> Set Dot
foldPaper (Horizontal along) = foldLeft along
foldPaper (Vertical along) = foldUp along

part1 :: Input -> Int
part1 Input { inputDots, inputInstructions }=
  Set.size . foldPaper (head inputInstructions) $ inputDots


part2 :: Input -> String
part2 Input { inputDots, inputInstructions } =
  let
    finalDots = foldl (flip foldPaper) inputDots inputInstructions
    -- Tuples sort on the first element, so we can't guarantee that if the min
    -- is (1,2) that there isn't a (2,1).  Hence we search the whole set.
    (minX, minY) = Set.foldr (\(x, y) (minX, minY) -> (min x minX, min y minY)) (Set.findMin finalDots) finalDots
    (maxX, maxY) = Set.foldr (\(x, y) (maxX, maxY) -> (max x maxX, max y maxY)) (Set.findMax finalDots) finalDots
    showLine y =
      map (\x -> if Set.member (x, y) finalDots then '#' else '.') [minX..maxX]
    showPaper =
      intercalate "\n" $ map showLine [minY..maxY] 
  in
    showPaper