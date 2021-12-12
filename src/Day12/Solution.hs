module Solution where

import qualified Data.Attoparsec.Text as P
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Exercise

startCave = "start"

endCave = "end"

type Cave = Text

-- Parsing
--------------------------------------------------

data Connection = Connection
  { from :: Cave,
    to :: Cave
  }
  deriving (Show, Eq)

type Input = [Connection]

connectionParser :: P.Parser Connection
connectionParser =
  Connection <$> P.takeWhile1 Char.isAlpha <* P.char '-' <*> P.takeWhile1 Char.isAlpha

inputParser :: P.Parser Input
inputParser =
  connectionParser `P.sepBy` P.endOfLine <* P.skipSpace <* P.endOfInput

-- Cave Map
--------------------------------------------------

newtype Caves = Caves
  { caveConnections :: Map Cave [Cave]
  }
  deriving (Show, Eq)

buildCaves :: Input -> Caves
buildCaves =
  Caves
    . Map.fromListWith (++)
    . concatMap (\(Connection from to) -> [(from, [to]), (to, [from])])

getConnections :: Caves -> Cave -> [Cave]
getConnections =
  (Map.!) . caveConnections

isBig :: Cave -> Bool
isBig = Text.all Char.isUpper

-- Visited Tracking
--------------------------------------------------

data VisitedCaves = VisitedCaves
  { extraVisitAllowed :: Bool,
    visitedSet :: Set Cave
  }
  deriving (Show, Eq)

emptyVisitedCaves :: Bool -> VisitedCaves
emptyVisitedCaves extraVisitAllowed =
  VisitedCaves
    { extraVisitAllowed = extraVisitAllowed,
      visitedSet = Set.empty
    }

isVisited :: Cave -> VisitedCaves -> Bool
isVisited cave VisitedCaves {extraVisitAllowed, visitedSet} =
    Set.member cave visitedSet
    && (cave == startCave || cave == endCave || not extraVisitAllowed)

visit :: Cave -> VisitedCaves -> VisitedCaves
visit cave VisitedCaves {extraVisitAllowed, visitedSet}
  | Set.member cave visitedSet =
    VisitedCaves {extraVisitAllowed = False, visitedSet}
  | otherwise =
    VisitedCaves {extraVisitAllowed, visitedSet = Set.insert cave visitedSet}

-- Path Finding
--------------------------------------------------

countPaths :: Bool -> Caves -> Int
countPaths extraVisitAllowed caves =
  countAllFrom caves (emptyVisitedCaves extraVisitAllowed) startCave

countAllFrom :: Caves -> VisitedCaves -> Cave -> Int
countAllFrom caves visited cave
  | cave == endCave = 1
  | isVisited cave visited = 0
  | otherwise =
    let connections = getConnections caves cave
        visited' =
          if isBig cave
            then visited
            else visit cave visited
        paths = map (countAllFrom caves visited') connections
     in sum paths

-- Main
--------------------------------------------------

part1 :: Input -> Int
part1 =
  countPaths False . buildCaves

part2 :: Input -> Int
part2 =
  countPaths True . buildCaves

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = part1,
      solutionPart2 = part2
    }