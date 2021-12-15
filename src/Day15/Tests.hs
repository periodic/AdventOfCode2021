module Tests where

import qualified Data.Map as Map
import qualified Data.Ix as Ix

import Solution

tests =
  [ (findMinPath (Map.singleton (1,1) 1), 1)
  , (findMinPath (Map.fromList $ zip (Ix.range ((1,1), (3,3))) [1,1..]), 5)
  , (findMinPath (Map.fromList $ zip (Ix.range ((1,1), (3,3))) [2,2..]), 10)
  , let
      smallValues = zip [(1,1), (1,2), (1,3), (2,3), (3,3)] [1,1..]
      largeValues = zip (Ix.range ((2,1), (3, 2))) [2,2..]
      input = Map.fromList $ smallValues ++ largeValues
    in (findMinPath input, 5)
  ]