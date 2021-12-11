module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise

type Input = ()

solution :: Solution Input
solution =
  Solution
    { solutionParser = P.failure,
      solutionPart1 = undefined,
      solutionPart2 = undefined
    }