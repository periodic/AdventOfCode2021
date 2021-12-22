module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise
import Control.Monad.State
import Data.Void
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

type Pos = Int

type Input = (Pos, Pos)

inputParser :: P.Parser Input
inputParser = do
  "Player 1 starting position: "
  p1 <- P.decimal
  P.endOfLine
  "Player 2 starting position: "
  p2 <- P.decimal
  return (p1, p2)

class Die a where
  roll :: a -> [(a, Int)]

data DeterministicDie = DeterministicDie {
  value :: Int,
  numRolls :: Int
} deriving (Show, Eq)

instance Die DeterministicDie where
  roll DeterministicDie { value, numRolls } =
    [(DeterministicDie { value = (value `mod` 100) + 1, numRolls = numRolls + 1 }, value)]

newDeterministicDie :: DeterministicDie
newDeterministicDie = DeterministicDie { value = 1, numRolls = 0 }

newtype DiracDie = DiracDie { quantumState :: Void } deriving (Show, Eq)

instance Die DiracDie where
  roll die = map (die,) [1,2,3]

newDiracDie :: DiracDie
newDiracDie = DiracDie { quantumState = undefined }

data GameState die = GameState {
  goal :: Int,
  player1 :: Pos,
  player2 :: Pos,
  player1Points :: Int,
  player2Points :: Int,
  die :: die
} deriving (Show, Eq)

playGame :: Die die => Int -> die -> (Pos, Pos) -> [GameState die]
playGame goal die (p1, p2) =
  execStateT (runGame playUntilWin) (GameState goal p1 p2 0 0 die)

newtype Game die a = Game {
  runGame :: StateT (GameState die) [] a
} deriving (Functor, Applicative, Monad, MonadState (GameState die))

rollDie :: Die die => Game die Int
rollDie = do
  die <- gets die
  (newDie, value) <- Game . lift $ roll die
  modify $ \st -> st { die = newDie }
  return value

gameIsWon :: GameState die -> Bool
gameIsWon GameState { goal, player1Points, player2Points } =
  player1Points >= goal || player2Points >= goal

play1 :: Die die => Game die ()
play1 = do
  GameState {..} <- get
  roll <- sum <$> replicateM 3 rollDie
  let pos1 = ((player1 + roll - 1) `mod` 10) + 1
  modify $ \st -> st { player1 = pos1, player1Points = player1Points + pos1 }

play2 :: Die die => Game die ()
play2 = do
  GameState {..} <- get
  roll <- sum <$> replicateM 3 rollDie
  let pos2 = ((player2 + roll - 1) `mod` 10) + 1
  modify $ \st -> st { player2 = pos2, player2Points = player2Points + pos2 }

playUntilWin :: Die die => Game die ()
playUntilWin = do
  play1
  st <- get
  if gameIsWon st
    then return ()
    else do
      play2
      st <- get
      if gameIsWon st
        then return ()
        else playUntilWin

part1 :: Input -> Int
part1 startingPos =
  let st = head $ playGame 1000 newDeterministicDie startingPos
      losingScore =
        if player1Points st > player2Points st
          then player2Points st
          else player1Points st
  in (numRolls . die $ st) * losingScore


data Memo = Memo {
  p1Pos :: Int,
  p2Pos   :: Int,
  p1Points :: Int,
  p2Points :: Int,
  isPlayer1 :: Bool
} deriving (Show, Eq, Ord)

data MemoizationTable = Map Memo (Int, Int)

memoTable :: Map Memo (Int, Int)
memoTable =
  Map.fromList $ do
    p1Pos <- [1..10]
    p2Pos <- [1..10]
    p1Points <- [0..31]
    p2Points <- [0..31]
    isPlayer1 <- [True, False]
    let memo = Memo { p1Pos, p2Pos, p1Points, p2Points, isPlayer1 }
        wins = calculateWins memo
    return (memo, wins)

rollFrequencies :: [(Int, Int)]
rollFrequencies =
  map (\rs -> (head rs, length rs)) . List.group . List.sort $ [x + y + z | x <- [1..3], y <- [1..3], z <- [1..3]]

calculateWins :: Memo -> (Int, Int)
calculateWins memo@Memo { p1Pos, p2Pos, p1Points, p2Points, isPlayer1 }
  | p1Points >= 21 = (1, 0)
  | p2Points >= 21 = (0, 1)
  | otherwise =
    sumPairs . map (\(r, f) -> (\(a, b) -> (a * f, b * f)) . (memoTable Map.!) $ r) . filter (isValidGame . fst) . map (\(r, f) -> (nextTurn memo r, f)) $ rollFrequencies
  where
    sumPairs :: [(Int, Int)] -> (Int, Int)
    sumPairs =
      foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0)
    isValidGame :: Memo -> Bool
    isValidGame Memo { p1Pos, p2Pos, p1Points, p2Points, isPlayer1 } =
      if p1Pos >= 1 && p1Pos <= 10 && p2Pos >= 1 && p2Pos <= 10
        then p1Points >= 0 && p2Points >= 0
        else error "Somehow positions are out of bounds"

nextTurn :: Memo -> Int -> Memo
nextTurn Memo { p1Pos, p2Pos, p1Points, p2Points, isPlayer1 } roll
  | isPlayer1 =
    let newPos = (p1Pos + roll - 1) `mod` 10 + 1 
    in Memo {
      p1Pos = newPos,
      p2Pos = p2Pos,
      p1Points = p1Points + newPos,
      p2Points = p2Points,
      isPlayer1 = False
    }
  | otherwise =
    let newPos = (p2Pos + roll - 1) `mod` 10 + 1 
    in Memo {
      p1Pos = p1Pos,
      p2Pos = newPos,
      p1Points = p1Points,
      p2Points = p2Points + newPos,
      isPlayer1 = True
    }

part2 :: Input -> Int
part2 startingPos =
  {-
  let states = playGame 21 newDiracDie startingPos
      p1Wins = length $ filter (\st -> player1Points st > player2Points st) states
      totalGames = length states
  in max p1Wins (totalGames - p1Wins)
  -}
  let (p1Wins, p2Wins) = memoTable Map.! Memo {
    p1Pos = fst startingPos,
    p2Pos = snd startingPos,
    p1Points = 0,
    p2Points = 0,
    isPlayer1 = True
  }
  in max p1Wins p2Wins

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = part1,
      solutionPart2 = part2
    }