module Common where

import System.Environment ( getArgs )
import System.Exit
import qualified Data.Attoparsec.Text as P
import Data.Text
import qualified Data.Text.IO as TextIO

loadInput :: IO Text
loadInput = do
    [path] <- getArgs
    TextIO.readFile path

parseInput :: P.Parser a -> Text -> IO a
parseInput parser input = do
    let result = P.parseOnly parser input
    case result of 
        Left err -> do
            putStr "Failed to parse input: "
            putStrLn err
            exitFailure
        Right value ->
            return value

loadAndParseInput :: P.Parser a -> IO a
loadAndParseInput parser = do
    contents <- loadInput
    parseInput parser contents
