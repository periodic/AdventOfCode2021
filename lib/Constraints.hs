module Constraints (solve) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid ( First(First, getFirst) )
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L

solve :: (Ord a, Ord b) => Map a (Set b) -> Maybe (Map a b)
solve =
  fmap M.fromList . solveList . M.toList

solveList :: (Ord a, Ord b) => [(a, Set b)] -> Maybe [(a, b)]
solveList options =
  let sortedByConstrainedness = L.sortOn (S.size . snd) options
      (key, values) = head sortedByConstrainedness  
      remainingOptions = tail sortedByConstrainedness
   in if L.null options
        then Just mempty 
        else getFirst . mconcat . map First $ do
          value <- S.toList values
          let filteredOptions = L.map (\(k, opts) -> (k, S.delete value opts)) remainingOptions
          let subSolution = solveList filteredOptions
          return $ ((key, value) :) <$> subSolution