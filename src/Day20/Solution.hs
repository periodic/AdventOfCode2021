module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise
import Data.Functor
import ParserHelpers
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as UArray
import Data.Ix
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

type Coord = (Int, Int)
data Image = Image { imageDefault :: Bool, imageBounds :: (Coord, Coord), imageData :: UArray Coord Bool }

instance Show Image where
  show image = 
    let ((minX, minY), (maxX, maxY)) = imageBounds image
        showRow y = map (\x -> if getPixel image (x, y) then '#' else '.') [minX..maxX]
    in unlines $ map showRow [minY..maxY]

getPixel :: Image -> Coord -> Bool
getPixel image c =
  let bounds = imageBounds image
  in if inRange bounds c
     then imageData image UArray.! c
     else imageDefault image

imageFromList :: [(Coord, Bool)] -> Image
imageFromList pixels =
  let minBound = fst $ minimumBy (comparing fst) pixels
      maxBound = fst $ maximumBy (comparing fst) pixels
  in Image {
    imageDefault = False,
    imageBounds = (minBound, maxBound),
    imageData = UArray.array (minBound, maxBound) pixels
  }

countLit :: Image -> Int
countLit =
  length . filter (id) . UArray.elems . imageData

type Algorithm = Vector Bool

algoFromList :: [Bool] -> Algorithm
algoFromList =
  Vector.fromList

lookupValue :: Algorithm -> Int -> Bool
lookupValue algo v =
  algo Vector.! v  

-- Parser
--------------------------------------------------

type Input = (Algorithm, Image)

entryParser :: P.Parser Bool
entryParser =
  P.choice [ "#" $> True, "." $> False ]

algoParser :: P.Parser Algorithm
algoParser =
  algoFromList . concat <$> P.many1 entryParser `P.sepBy` P.endOfLine

imageParser :: P.Parser Image
imageParser =
  imageFromList <$> parseGrid (P.many1 entryParser)

inputParser :: P.Parser Input
inputParser = do
  algo <- algoParser
  P.endOfLine
  P.endOfLine
  image <- imageParser
  return (algo,image)

-- Part 1
--------------------------------------------------

getValue :: Image -> Coord -> Int
getValue image (x, y) =
  let coords = [(x', y') | y' <- [y-1..y+1], x' <- [x-1..x+1]]
  in foldr (\p acc -> 2 * acc + if getPixel image p then 1 else 0) 0 coords

enhance :: Algorithm -> Image -> Image
enhance algo image =
  let ((minX, minY), (maxX, maxY)) = imageBounds image
      coords = [(x, y) | y <- [minY-1..maxY+1], x <- [minX-1..maxX+1]]
      defaultValue = getValue image (minX - 1000, minY - 1000)
      newDefault = lookupValue algo defaultValue
  in imageFromList $ map (\c -> (c, lookupValue algo . getValue image $ c)) coords

part1 :: Input -> Int
part1 (algo, image) =
  countLit . enhance algo $ enhance algo image

part2 :: Input -> Int
part2 (algo, image) =
  countLit $ foldr ($) image (replicate 50 (enhance algo))

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = part1,
      solutionPart2 = part2
    }