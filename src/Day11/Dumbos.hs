module Dumbos where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

type Coord = (Int, Int)

type DumboState s = STUArray s Coord Int

dumboBounds = ((1, 1), (10, 10))
energyThreshold = 10

newtype Dumbo s a = Dumbo
  { runDumbo :: ReaderT (DumboState s) (ST s) a
  }
  deriving (Functor, Applicative, Monad, MonadReader (DumboState s))

liftST = Dumbo . lift

fromMap :: forall s. Map Coord Int -> ST s (DumboState s)
fromMap mappedData = do
  array <- newArray dumboBounds 0
  mapM_ (uncurry $ writeArray array) (Map.toList mappedData)
  return array

toMap :: forall s. DumboState s -> ST s (Map Coord Int)
toMap array =
  Map.fromList <$> getAssocs array

neighbors :: Coord -> [Coord]
neighbors (x, y) =
  [(x - 1, y -1), (x, y -1), (x + 1, y -1), (x -1, y), (x + 1, y), (x -1, y + 1), (x, y + 1), (x + 1, y + 1)]

addEnergy :: forall s. Coord -> Dumbo s ()
addEnergy coord = do
  array <- ask
  energy <- liftST $ readArray array coord
  liftST $ writeArray array coord (energy + 1)
  when (energy + 1 == energyThreshold)
    . mapM_ addEnergy . filter (inRange dumboBounds) $ neighbors coord

cleanAndCountFlashes :: forall s. Dumbo s Int
cleanAndCountFlashes = do
  array <- ask
  sum <$> mapM cleanAndCountFlash (range dumboBounds)
  where
    cleanAndCountFlash coord = do
      array <- ask
      energy <- liftST $ readArray array coord
      if energy >= energyThreshold
        then do
          liftST $ writeArray array coord 0
          return 1
        else return 0

runCycle :: forall s. Dumbo s Int
runCycle = do
  array <- ask
  mapM_ addEnergy (range dumboBounds)
  cleanAndCountFlashes

runCycles :: forall s. Int -> Dumbo s Int
runCycles n = foldM (\acc _ -> (+acc) <$> runCycle) 0 [1..n]

runUntilAllFlash :: forall s. Dumbo s Int
runUntilAllFlash =
  let target = rangeSize dumboBounds
  in untilM 0 (== target) runCycle
  where
    untilM n pred m = do
      acc <- m
      if pred acc
        then return (n + 1)
        else untilM (n + 1) pred m

runCyclesFromMap :: Int -> Map Coord Int -> (Int, Map Coord Int)
runCyclesFromMap n = 
  runFromMap (runCycles n)

runFromMap :: (forall s. Dumbo s Int) -> Map Coord Int -> (Int, Map Coord Int)
runFromMap action dataMap = runST $ do
  state <- fromMap dataMap
  result <- runReaderT (runDumbo action) state
  finalDataMap <- toMap state
  return (result, finalDataMap)