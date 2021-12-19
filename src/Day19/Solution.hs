module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise
import Linear.V3
import qualified Data.List as List
import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

type Coord = V3 Int

data ScannerReadings = ScannerReadings {
  scannerReadingsId :: Int,
  readingsToList :: [Coord]
} deriving (Show, Eq)

-- Parsing
--------------------------------------------------

type Input = [ScannerReadings]

readingParser :: P.Parser Coord
readingParser =
  V3
  <$> P.signed P.decimal
  <* P.char ','
  <*> P.signed P.decimal
  <* P.char ','
  <*> P.signed P.decimal

scannerParser :: P.Parser ScannerReadings
scannerParser = do
  id <- "--- scanner " *> P.decimal <* " ---"
  P.endOfLine
  readings <- readingParser `P.sepBy` P.endOfLine
  return $ ScannerReadings id readings

inputParser :: P.Parser Input
inputParser = scannerParser `P.sepBy` (P.endOfLine *> P.endOfLine)

-- Rotations
--------------------------------------------------

data RotationStep
  = ZCW
  | ZCCW
  | XCW
  | XCCW
  | YCW
  | YCCW
  deriving (Show, Eq)

data Rotation
  = R RotationStep Rotation
  | NilR
  deriving (Show, Eq)

r :: RotationStep -> Rotation
r step = R step NilR

concatRotations :: Rotation -> Rotation -> Rotation
concatRotations NilR r = r
concatRotations (R step rest) r = R step (concatRotations rest r)

zOrientations :: [Rotation]
zOrientations = [NilR, r XCW, r XCCW, r YCW, r YCCW, R XCW (r XCW)]

zRotations :: [Rotation]
zRotations = [NilR, r ZCW, r ZCCW, R ZCW (r ZCW)]

allOrientations :: [Rotation]
allOrientations =
  [ concatRotations orientation rotation | orientation <- zOrientations, rotation <- zRotations ]

rotatePoint :: Rotation -> Coord -> Coord
rotatePoint NilR p = p
rotatePoint (R XCCW r) (V3 x y z) = rotatePoint r $ V3 x (negate z) y
rotatePoint (R XCW r) (V3 x y z) = rotatePoint r $ V3 x z (negate y)
rotatePoint (R YCCW r) (V3 x y z) = rotatePoint r $ V3 z y (negate x)
rotatePoint (R YCW r) (V3 x y z) = rotatePoint r $ V3 (negate z) y x
rotatePoint (R ZCCW r) (V3 x y z) = rotatePoint r $ V3 (negate y) x z
rotatePoint (R ZCW r) (V3 x y z) = rotatePoint r $ V3 y (negate x) z

-- Scanner Processing
--------------------------------------------------

data Scanner = Scanner {
  scannerId :: Int,
  scannerPos :: Coord,
  scannerOrientation :: Rotation,
  scannerPoints :: Set Coord
} deriving (Show, Eq)

instance Ord Scanner where
  a `compare` b = scannerId a `compare` scannerId b

scanner :: ScannerReadings -> Scanner
scanner readings =
  Scanner {
    scannerId = (scannerReadingsId readings),
    scannerPos = (V3 0 0 0),
    scannerOrientation = NilR,
    scannerPoints = Set.fromList $ readingsToList readings
  }

getPoints :: Scanner -> Set Coord
getPoints Scanner { scannerPos, scannerOrientation, scannerPoints } =
  Set.map ((+ scannerPos) . rotatePoint scannerOrientation) scannerPoints

rotate :: RotationStep -> Scanner -> Scanner
rotate r scanner =
  scanner {
    scannerOrientation = R r (scannerOrientation scanner)
  }

setRotation :: Rotation -> Scanner -> Scanner
setRotation r scanner =
  scanner {
    scannerOrientation = r
  }

translate :: Coord -> Scanner -> Scanner
translate v scanner =
  scanner {
    scannerPos = scannerPos scanner + v
  }

-- Scanner Comparisons
--------------------------------------------------

overlapThreshold = 12

findOverlap :: Scanner -> Scanner -> Maybe Coord
findOverlap base cand =
  let basePoints = getPoints base
  in listToMaybe $ do
      basePoint <- Set.toList basePoints
      candPoint <- Set.toList $ getPoints cand
      let offset = basePoint - candPoint
          shiftedCand = translate offset cand
          overlap = Set.intersection basePoints $ (getPoints shiftedCand) 
      guard $ Set.size overlap >= overlapThreshold
      return offset

orient :: Scanner -> Scanner -> Maybe Scanner
orient base cand =
  listToMaybe . catMaybes . map findCand $ allOrientations
  where
    findCand :: Rotation -> Maybe Scanner
    findCand rot = do
      let rotatedCandidate = setRotation rot cand
      offset <- findOverlap base rotatedCandidate
      return $ translate offset rotatedCandidate

combineAllInto :: Set Scanner -> Set Scanner -> Maybe (Set Scanner)
combineAllInto baseSet candidates 
  | Set.null candidates = Just baseSet
  | otherwise = do
    (newBase, newCandidates) <- tryAll baseSet candidates
    combineAllInto newBase newCandidates

tryAll :: Set Scanner -> Set Scanner -> Maybe (Set Scanner, Set Scanner)
tryAll bases candidates = do
  orientedCandidate <- listToMaybe . catMaybes $ do
    candidate <- Set.toList candidates
    base <- Set.toList bases
    return $ orient base candidate
  return (Set.insert orientedCandidate bases, Set.delete orientedCandidate candidates)

orientAll :: [Scanner] -> Set Scanner
orientAll [] = Set.empty
orientAll (base:scanners) = 
  fromMaybe (error "Unable to find a solution") $ combineAllInto (Set.singleton base) (Set.fromList scanners)

countBeacons :: [Scanner] -> Int
countBeacons =
  Set.size . Set.foldr (Set.union . getPoints) Set.empty . orientAll

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = countBeacons . map scanner,
      solutionPart2 = undefined
    }