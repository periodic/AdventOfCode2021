module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise

type Input = String 

solution :: Solution Input
solution =
  Solution
    { solutionParser = P.many1 P.anyChar,
      solutionPart1 = const $ (124 * 123) `div` 2,
      solutionPart2 = undefined
    }