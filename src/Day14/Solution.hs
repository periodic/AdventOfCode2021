module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Extra (maximumOn, minimumOn)
import qualified Data.Text as Text

data Rule = Rule {
  rulePair :: (Char, Char),
  ruleInsertion :: Char
}

instance Show Rule where
  show (Rule (a, b) c) = [a, b] ++ " -> " ++ [c]

data Input = Input
  { inputTemplate :: String
  , inputRules :: [Rule]
  } deriving (Show)

ruleParser :: P.Parser Rule
ruleParser = do
  a <- P.letter
  b <- P.letter
  P.string " -> "
  Rule (a, b) <$> P.letter

templateParser :: P.Parser String
templateParser = Text.unpack <$> P.takeWhile Char.isAlpha

inputParser = do
  template <- templateParser
  P.skipSpace
  rules <- ruleParser `P.sepBy1` P.endOfLine
  return $ Input template rules

type RuleMap = Map (Char, Char) Char

rulesToMap :: [Rule] -> RuleMap
rulesToMap = Map.fromList . map (\(Rule (a, b) c) -> ((a, b), c))

data Polymer = Polymer
  { polymerPairs :: Map (Char, Char) Int
  , polymerCounts :: Map Char Int
  } deriving (Show)

stringToPolymer :: String -> Polymer
stringToPolymer [] = Polymer Map.empty Map.empty
stringToPolymer [a] = Polymer Map.empty (Map.singleton a 1)
stringToPolymer (a:b:xs) =
  let
    (Polymer pairs counts) = stringToPolymer (b:xs)
  in
    Polymer
      (Map.insertWith (+) (a, b) 1 pairs)
      (Map.insertWith (+) a 1 counts)

applyRules :: RuleMap -> Polymer -> Polymer
applyRules rules polymer =
  foldr processPair polymer . Map.assocs . polymerPairs $ polymer
  where
    processPair (pair@(a, b), count) polymer =
      case Map.lookup pair rules of
        Nothing -> polymer
        Just c ->
          let
            newPairs =
                Map.insertWith (+) pair (-count)
                . Map.insertWith (+) (a, c) count
                . Map.insertWith (+) (c, b) count
                $ polymerPairs polymer
            newCounts =
              Map.insertWith (+) c count $ polymerCounts polymer
          in
            Polymer newPairs newCounts

runAndCount :: Int -> Input -> Int
runAndCount iters input =
  let
    rules = rulesToMap $ inputRules input
    initial = stringToPolymer $ inputTemplate input
    final = iterate (applyRules rules) initial !! iters
    charCounts = polymerCounts final
    mostCommonCount = snd $ maximumOn snd (Map.toList charCounts)
    leastCommonCount = snd $ minimumOn snd (Map.toList charCounts)
  in 
    mostCommonCount - leastCommonCount

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = runAndCount 10,
      solutionPart2 = runAndCount 40
    }