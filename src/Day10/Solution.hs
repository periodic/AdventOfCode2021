module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise ( Solution(..) )
import Data.Maybe (mapMaybe)
import Data.Attoparsec.Internal.Types (Chunk)
import Control.Lens (Field12(_12))
import Data.List (sort)

data ChunkType
  = Paren
  | Bracket
  | Brace
  | Angle
  deriving (Ord, Eq)

data Character
  = Open ChunkType
  | Close ChunkType
  deriving (Ord, Eq)

instance Show Character where
  show (Open Paren) = "("
  show (Close Paren) = ")"
  show (Open Bracket) = "["
  show (Close Bracket) = "]"
  show (Open Brace) = "{"
  show (Close Brace) = "}"
  show (Open Angle) = "<"
  show (Close Angle) = ">"

type Line = [Character]
type Program = [Line]

parseCharacter :: P.Parser Character
parseCharacter =
  P.choice
    [ P.string "(" >> return (Open Paren)
    , P.string ")" >> return (Close Paren)
    , P.string "[" >> return (Open Bracket)
    , P.string "]" >> return (Close Bracket)
    , P.string "{" >> return (Open Brace)
    , P.string "}" >> return (Close Brace)
    , P.string "<" >> return (Open Angle)
    , P.string ">" >> return (Close Angle)
    ]

parser :: P.Parser Program
parser =
  P.many1 parseCharacter `P.sepBy` P.endOfLine
  <* P.skipSpace <* P.endOfInput

data ParseResult
  = Success
  | Incomplete [ChunkType]
  | InvalidClose ChunkType

analyzeLine :: [Character] -> ParseResult
analyzeLine =
  analyzeLineWithStack []
  where
    analyzeLineWithStack [] [] = Success
    analyzeLineWithStack stack [] = Incomplete stack
    analyzeLineWithStack stack (Open c : cs) = analyzeLineWithStack (c : stack) cs
    analyzeLineWithStack [] (Close c : cs) = InvalidClose c
    analyzeLineWithStack (s:ss) (Close c : cs) =
      if s /= c
        then InvalidClose c
        else analyzeLineWithStack ss cs

scoreIllegalCharacter :: ChunkType -> Int
scoreIllegalCharacter Paren = 3
scoreIllegalCharacter Bracket = 57
scoreIllegalCharacter Brace = 1197
scoreIllegalCharacter Angle = 25137

scoreErrors :: ParseResult -> Int
scoreErrors Success = 0
scoreErrors (Incomplete _) = 0
scoreErrors (InvalidClose c) = scoreIllegalCharacter c

part1 :: Program -> Int
part1 =
  sum . map (scoreErrors . analyzeLine)

scoreIncomplete :: ParseResult -> Int
scoreIncomplete Success = 0
scoreIncomplete (InvalidClose _) = 0
scoreIncomplete (Incomplete stack) =
  foldl (\acc c -> acc * 5 + scoreIncompleteChunk c) 0 stack
  where
    scoreIncompleteChunk Paren = 1
    scoreIncompleteChunk Bracket = 2
    scoreIncompleteChunk Brace = 3
    scoreIncompleteChunk Angle = 4

part2 :: Program -> Int
part2 program =
  let
    scores = filter (>0) . map (scoreIncomplete . analyzeLine) $ program
    middle = length scores `div` 2
  in
    sort scores !! middle

solution :: Solution Program
solution =
  Solution
    { solutionParser = parser,
      solutionPart1 = part1,
      solutionPart2 = part2
    }

