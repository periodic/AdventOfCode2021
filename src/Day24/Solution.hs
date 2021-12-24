module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Functor

data Var = W | X | Y | Z deriving (Eq, Ord, Show)
data Val = Var Var | Const Int deriving (Eq, Ord, Show)
data Instruction
  = Input Var
  | Add Var Val
  | Mul Var Val
  | Div Var Val
  | Mod Var Val
  | Eql Var Val
  deriving (Show, Eq)

type Program = [Instruction]

type Input = Program

instructionParser :: P.Parser Instruction
instructionParser =
  P.choice
    [ P.string "inp " *> (Input <$> varParser)
    , P.string "add " *> (Add <$> varParser <*> valParser)
    , P.string "mul " *> (Mul <$> varParser <*> valParser)
    , P.string "div " *> (Div <$> varParser <*> valParser)
    , P.string "mod " *> (Mod <$> varParser <*> valParser)
    , P.string "eql " *> (Eql <$> varParser <*> valParser)
    ]
  where
    varParser = P.skipSpace *> P.choice [
      "w" $> W,
      "x" $> X,
      "y" $> Y,
      "z" $> Z
      ]
    constParser = P.skipSpace *> P.signed P.decimal
    valParser = P.choice [ Var <$> varParser, Const <$> constParser ]

programParser :: P.Parser Program
programParser =
  instructionParser `P.sepBy` P.endOfLine

data AluState = AluState
  { registers :: Map Var Int
  , aluInput :: [Int]
  } deriving (Show)

emptyState :: [Int] -> AluState
emptyState = AluState Map.empty 

newtype AluIO a = AluIO {
  runAluIO :: State AluState a
  } deriving (Functor, Applicative, Monad, MonadState AluState)

setVar :: Var -> Int -> AluIO ()
setVar reg val =
  modify (\s -> s { registers = Map.insert reg val (registers s) })

getVar :: Var -> AluIO Int
getVar reg =
  gets (Map.findWithDefault 0 reg . registers)

getVal :: Val -> AluIO Int
getVal (Var reg) = getVar reg
getVal (Const val) = pure val

readValue :: AluIO Int
readValue = do
  s <- get
  case aluInput s of
    [] -> error "No input"
    (x:xs) -> do
      put s { aluInput = xs }
      return x

runInstruction :: Instruction -> AluIO ()
runInstruction (Input r) = do
  val <- readValue
  setVar r val
runInstruction (Add r v) = do
  val1 <- getVar r
  val2 <- getVal v
  setVar r (val1 + val2)
runInstruction (Mul r v) = do
  val1 <- getVar r
  val2 <- getVal v
  setVar r (val1 * val2)
runInstruction (Div r v) = do
  val1 <- getVar r
  val2 <- getVal v
  setVar r (val1 `div` val2)
runInstruction (Mod r v) = do
  val1 <- getVar r
  val2 <- getVal v
  setVar r (val1 `mod` val2)
runInstruction (Eql r v) = do
  val1 <- getVar r
  val2 <- getVal v
  setVar r (if val1 == val2 then 1 else 0)

runProgram :: Program -> [Int] -> AluState
runProgram prog input =
  let (AluIO m) = mapM_ runInstruction prog
  in execState m (emptyState input)

isAcceptedModel :: Program -> [Int] -> Bool
isAcceptedModel prog =
  (== 0) . Map.findWithDefault 1 Z . registers . runProgram prog

findLargestAccepted :: Program -> Maybe Int
findLargestAccepted prog =
  fmap combine . List.find (isAcceptedModel prog) $ genInput 14
  where
    combine = foldl (\acc x -> acc * 10 + x) 0

genInput :: Int -> [[Int]]
genInput 1 = map (:[]) [9,8..1]
genInput i = do
  d <- [9,8..1]
  map (d:) $ genInput (i - 1)

solution :: Solution Input
solution =
  Solution
    { solutionParser = programParser,
      solutionPart1 = Maybe.fromMaybe (error "Nothing accepted") . findLargestAccepted,
      solutionPart2 = undefined
    }