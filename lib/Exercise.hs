module Exercise (runExercise, readInput, parseInput) where

import Control.Monad
import Options.Applicative
import System.Exit
import qualified Data.Attoparsec.Text as Attoparsec
import System.CPUTime
import Text.Printf
import Data.Text (Text)
import Data.Text.IO ( readFile )
import Criterion

-- TODO: Clean up this file.

inputFileArg :: Parser String
inputFileArg =
  strArgument (metavar "INPUT_FILE")

benchmarkArg :: Parser Bool
benchmarkArg =
  switch (long "benchmark" <> short 'b' <> help "Enable detailed benchmarking")

argsParser :: Parser Arguments
argsParser =
  Arguments <$> inputFileArg <*> benchmarkArg

argsInfo :: ParserInfo Arguments
argsInfo =
  info argsParser mempty

data Arguments = Arguments {
  inputFile :: String,
  benchmarking :: Bool
} deriving Show

parseArgs :: IO Arguments
parseArgs =
  execParser argsInfo

readInput :: IO Text
readInput = do
  args <- parseArgs
  Data.Text.IO.readFile . inputFile $ args

parseInput :: Attoparsec.Parser a -> IO a
parseInput parser = do
  printf "Parsing input...\n"
  args <- parseArgs
  contents <- readInput
  time "Parsing" (doParsing parser) contents

doParsing :: Attoparsec.Parser a -> Text -> IO a
doParsing parser contents =
  case Attoparsec.parseOnly parser contents of
    Left err -> do
      printf "Failed to parse input: %s\n" err
      exitFailure
    Right input ->
      return input

runExercise :: String -> (a -> b) -> a -> IO b
runExercise name work input = do
  printSeparator
  printf "Running %s...\n" name
  time name (return . work) input

printTimeDiff :: Integer -> Integer -> IO ()
printTimeDiff start end = do
  let diff = fromIntegral (end - start) / (10 ^ 9) :: Double
  printf "Work took %0.3fms\n" diff

printSeparator :: IO ()
printSeparator =
  putStrLn . replicate 80 $ '='

time :: String -> (a -> IO b) -> a -> IO b
time name work input = do
  args <- parseArgs
  if benchmarking args
    then do
      benchmark $ whnfAppIO work input
      work input
    else do
      start <- getCPUTime 
      result <- work input
      end <- result `seq` getCPUTime
      printTimeDiff start end
      return result
