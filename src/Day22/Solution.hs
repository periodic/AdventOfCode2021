module Solution where

import qualified Data.Attoparsec.Text as P
import Data.Functor
import qualified Data.Ix as Ix
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Exercise

-- Regions
--------------------------------------------------

data Region = Region
  { xMin :: Int,
    xMax :: Int,
    yMin :: Int,
    yMax :: Int,
    zMin :: Int,
    zMax :: Int
  }
  deriving (Show, Eq, Ord)

regionSize :: Region -> Int
regionSize Region {..} =
  (xMax - xMin + 1) * (yMax - yMin + 1) * (zMax - zMin + 1)

overlapping :: Region -> Region -> Bool
overlapping r1 r2 =
  xMin r1 <= xMax r2 && xMax r1 >= xMin r2
    && yMin r1 <= yMax r2
    && yMax r1 >= yMin r2
    && zMin r1 <= zMax r2
    && zMax r1 >= zMin r2

subtractRegion :: Region -> Region -> [Region]
subtractRegion source difference =
  if not (overlapping source difference)
    then [source]
    else splitRegion
  where
    -- Split it up by restricting in each dimension in turn.
    splitRegion = xRegions ++ yRegions ++ zRegions
    (xIntersectMin, xIntersectMax) = (max (xMin source) (xMin difference), min (xMax source) (xMax difference))
    (yIntersectMin, yIntersectMax) = (max (yMin source) (yMin difference), min (yMax source) (yMax difference))
    (zIntersectMin, zIntersectMax) = (max (zMin source) (zMin difference), min (zMax source) (zMax difference))
    xRegions = do
      (xMin, xMax) <- filter (uncurry (<=)) [(xMin source, xMin difference - 1), (xMax difference + 1, xMax source)]
      [Region {xMin, xMax, yMin = yMin source, yMax = yMax source, zMin = zMin source, zMax = zMax source}]
    yRegions = do
      (yMin, yMax) <- filter (uncurry (<=)) [(yMin source, yMin difference - 1), (yMax difference + 1, yMax source)]
      [Region {xMin = xIntersectMin, xMax = xIntersectMax, yMin, yMax, zMin = zMin source, zMax = zMax source}]
    zRegions = do
      (zMin, zMax) <- filter (uncurry (<=)) [(zMin source, zMin difference - 1), (zMax difference + 1, zMax source)]
      [Region {xMin = xIntersectMin, xMax = xIntersectMax, yMin = yIntersectMin, yMax = yIntersectMax, zMin, zMax}]

-- Parsing
--------------------------------------------------

data BootStep = BootStep
  { state :: Bool,
    region :: Region
  }
  deriving (Show, Eq, Ord)

type Input = [BootStep]

stepParser :: P.Parser BootStep
stepParser = do
  state <- P.choice ["on" $> True, "off" $> False]
  " x="
  xMin <- P.signed P.decimal
  ".."
  xMax <- P.signed P.decimal
  ",y="
  yMin <- P.signed P.decimal
  ".."
  yMax <- P.signed P.decimal
  ",z="
  zMin <- P.signed P.decimal
  ".."
  zMax <- P.signed P.decimal
  return $ BootStep {state, region = Region {xMin, xMax, yMin, yMax, zMin, zMax}}

inputParser :: P.Parser Input
inputParser =
  stepParser `P.sepBy` P.endOfLine

-- Reactor
--------------------------------------------------

newtype Reactor = Reactor
  { getRegions :: [Region]
  }
  deriving (Show)

runStep :: BootStep -> Reactor -> Reactor
runStep BootStep {..} (Reactor regions) =
  -- TODO: Skip the new step if everything is already on
  -- TODO: Merge regions?
  Reactor $ (if state then [region] else []) ++ (concatMap (`subtractRegion` region) regions)

boot :: [BootStep] -> Reactor
boot = foldl (flip runStep) (Reactor [])

restrictToRegion :: (Int, Int, Int) -> (Int, Int, Int) -> BootStep -> Maybe BootStep
restrictToRegion (xMinR, yMinR, zMinR) (xMaxR, yMaxR, zMaxR) step@BootStep {region = Region {..}} =
  let newXMin = max xMin xMinR
      newXMax = min xMax xMaxR
      newYMin = max yMin yMinR
      newYMax = min yMax yMaxR
      newZMin = max zMin zMinR
      newZMax = min zMax zMaxR
   in if xMin > xMaxR || xMax < xMinR
        || yMin > yMaxR
        || yMax < yMinR
        || zMin > zMaxR
        || zMax < zMinR
        then Nothing
        else
          Just $
            step {region = Region {xMin = newXMin, xMax = newXMax, yMin = newYMin, yMax = newYMax, zMin = newZMin, zMax = newZMax}}

countOn :: Reactor -> Int
countOn (Reactor regions) =
  sum $ map regionSize regions

-- Solution
--------------------------------------------------

part1 :: Input -> Int
part1 = countOn . boot . Maybe.mapMaybe (restrictToRegion (-50, -50, -50) (50, 50, 50))

part2 :: Input -> Int
part2 = countOn . boot

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = part1,
      solutionPart2 = part2
    }