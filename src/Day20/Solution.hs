module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise
import Data.Functor
import ParserHelpers
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

type Coord = (Int, Int)
data Image = Image { imageDefault :: Bool, imageData :: Map Coord Bool }
type Algorithm = Map Int Bool
type Input = ([Bool], Image)

instance Show Image where
  show image = 
    let ((minX, minY), (maxX, maxY)) = imageBounds image
        showRow y = map (\x -> if getPixel image (x, y) then '#' else '.') [minX..maxX]
    in unlines $ map showRow [minY..maxY]

imageBounds :: Image -> ((Int, Int), (Int, Int))
imageBounds (Image _ m) =
  let ((minX, minY), _) = Map.findMin m
      ((maxX, maxY), _) = Map.findMax m
  in ((minX, minY), (maxX, maxY))

getPixel :: Image -> Coord -> Bool
getPixel image c =
  Map.findWithDefault (imageDefault image) c (imageData image)

-- Parser
--------------------------------------------------

entryParser :: P.Parser Bool
entryParser =
  P.choice [ "#" $> True, "." $> False ]

algoParser :: P.Parser [Bool]
algoParser =
  concat <$> P.many1 entryParser `P.sepBy` P.endOfLine

imageParser :: P.Parser Image
imageParser =
  Image False . Map.fromList <$> parseGrid (P.many1 entryParser)

inputParser :: P.Parser Input
inputParser = do
  algo <- algoParser
  P.endOfLine
  P.endOfLine
  image <- imageParser
  return (algo,image)

algoToMap :: [Bool] -> Map Int Bool
algoToMap =
  Map.fromList . zip [0..]

-- Part 1
--------------------------------------------------

getValue :: Image -> Coord -> Int
getValue image (x, y) =
  let coords = [(x', y') | y' <- [y-1..y+1], x' <- [x-1..x+1]]
      pixels = map (getPixel image) coords
  in foldl (\acc p -> 2 * acc + if p then 1 else 0) 0 pixels

lookupValue :: Algorithm -> Int -> Bool
lookupValue algo v =
  Map.findWithDefault (error $ "Value not in algorithm: " ++ show v) v algo

enhance :: Algorithm -> Image -> Image
enhance algo image =
  let ((minX, minY), (maxX, maxY)) = imageBounds image
      coords = [(x, y) | y <- [minY-1..maxY+1], x <- [minX-1..maxX+1]]
      defaultValue = getValue image (minX - 1000, minY - 1000)
      newDefault = lookupValue algo defaultValue
  in Image newDefault . Map.fromList $ map (\c -> (c, lookupValue algo . getValue image $ c)) coords

countLit :: Image -> Int
countLit =
  Map.foldl (\acc p -> acc + if p then 1 else 0) 0 . imageData

part1 :: Input -> Int
part1 (algo, image) =
  let algoMap = algoToMap algo
  in countLit . enhance algoMap $ enhance algoMap image

part2 :: Input -> Int
part2 (algo, image) =
  let algoMap = algoToMap algo
  in countLit $ foldr ($) image (replicate 50 (enhance algoMap))

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = part1,
      solutionPart2 = part2
    }