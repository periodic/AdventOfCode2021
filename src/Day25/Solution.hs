{-# LANGUAGE MagicHash #-}
module Solution where

import qualified Data.Attoparsec.Text as P
import Exercise
import ParserHelpers
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as IArray
import Data.Array.ST (STUArray)
import qualified Data.Array.ST as MArray
import qualified Data.Ix as Ix
import Control.Monad.State
import Control.Monad.ST
import qualified Data.Array.Base as Base
import qualified Data.Primitive.ByteArray as ByteArray
import GHC.Exts (compareByteArrays#, Int(..))


type Cucumber = Char
newtype SeaFloor = SeaFloor {
  toArray :: UArray (Int, Int) Cucumber
}

instance Show SeaFloor where
  show (SeaFloor a) = 
    let ((minX, minY), (maxX, maxY)) = IArray.bounds a
        showRow y = 
          map (\x -> a IArray.! (x, y)) [minX..maxX]
    in unlines $ map showRow [minY..maxY]

copyArray :: forall s. STUArray s (Int, Int) Cucumber -> STUArray s (Int, Int) Cucumber -> ST s ()
copyArray source destination =
  MArray.getAssocs source >>= mapM_ (uncurry $ MArray.writeArray destination)

-- | Compares two sea floor arrays.  Assumed they are the same size
compareArrays :: forall s. STUArray s (Int, Int) Cucumber -> STUArray s (Int, Int) Cucumber -> ST s Bool
compareArrays arr1 arr2 = do
  Base.UArray _ _ _ frozen1# <- Base.unsafeFreezeSTUArray arr1
  Base.UArray l u _ frozen2# <- Base.unsafeFreezeSTUArray arr2
  let I# n# = Ix.rangeSize (l, u)
  return $ I# (compareByteArrays# frozen1# 0# frozen2# 0# n#) == 0

-- Parser
--------------------------------------------------

type Input = SeaFloor

cucumberParser = P.satisfy (\c -> c `elem` ['.', 'v', '>'])

seaFloorParser = do
  assocs <- parseGrid (P.many1 cucumberParser)
  let minBound = minimum $ map fst assocs
      maxBound = maximum $ map fst assocs
      bounds = (minBound, maxBound)
  return . SeaFloor $ IArray.array bounds assocs

-- Cucumber Monad
--------------------------------------------------

type STSeaFloor s = STUArray s (Int, Int) Cucumber

data CucumberState s = CucumberState {
  iterations :: Int,
  currSeaFloor :: STSeaFloor s,
  nextSeaFloor :: STSeaFloor s
}

newtype CucumberM s a = CucumberM {
  runCucumberM :: StateT (CucumberState s) (ST s) a
} deriving (Functor, Applicative, Monad, MonadState (CucumberState s))

liftST = CucumberM . lift

clipToBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> (Int, Int)
clipToBounds ((minX, minY), (maxX, maxY)) (x, y) =
  ((x - minX) `mod` (maxX - minX + 1) + minX, (y - minY) `mod` (maxY - minY + 1) + minY)

getBounds :: forall s. CucumberM s ((Int, Int), (Int, Int))
getBounds = do
  CucumberState { currSeaFloor } <- get
  liftST $ MArray.getBounds currSeaFloor

getCuc :: forall s. (Int, Int) -> CucumberM s Cucumber
getCuc (x, y) = do
  bounds <- getBounds
  sf <- gets currSeaFloor
  liftST $ MArray.readArray sf $ clipToBounds bounds (x, y)

setCuc :: forall s. (Int, Int) -> Cucumber -> CucumberM s ()
setCuc (x, y) c = do
  bounds <- getBounds
  sf <- gets nextSeaFloor
  liftST $ MArray.writeArray sf (clipToBounds bounds (x, y)) c

finalizeMovement :: forall s. CucumberM s ()
finalizeMovement = do
  sf <- gets currSeaFloor
  sf' <- gets nextSeaFloor
  liftST $ copyArray sf' sf

markIteration :: forall s. CucumberM s ()
markIteration =
  modify $ \s -> s { iterations = iterations s + 1 }

runCucumbers :: (forall s. CucumberM s ()) -> SeaFloor -> (SeaFloor, Int)
runCucumbers behavior (SeaFloor sf) =
  runST $ do
    currSF <- MArray.thaw sf
    nextSF <- MArray.thaw sf
    let st = CucumberState {
          iterations = 0,
          currSeaFloor = currSF,
          nextSeaFloor = nextSF
        }
    st' <- execStateT (runCucumberM behavior) st
    frozenFloor <- SeaFloor <$> MArray.freeze currSF
    return (frozenFloor, iterations st')

-- Behavior
--------------------------------------------------

moveAll :: forall s. CucumberM s ()
moveAll = do
  bounds <- getBounds
  mapM_ moveEast $ Ix.range bounds
  finalizeMovement
  mapM_ moveSouth $ Ix.range bounds
  finalizeMovement
  markIteration

moveEast :: (Int, Int) -> CucumberM s ()
moveEast (x, y) = do
  c <- getCuc (x, y)
  nextSpace <- getCuc (x+1, y)
  case (c, nextSpace) of
    ('>', '.') -> do
      setCuc (x, y) '.'
      setCuc (x+1, y) '>'
    _ -> return ()

moveSouth :: (Int, Int) -> CucumberM s ()
moveSouth (x, y) = do
  c <- getCuc (x, y)
  nextSpace <- getCuc (x, y+1)
  case (c, nextSpace) of
    ('v', '.') -> do
      setCuc (x, y) '.'
      setCuc (x, y+1) 'v'
    _ -> return ()

moveUntilStable :: forall s. CucumberM s ()
moveUntilStable = do
  bounds <- getBounds
  lastSF <- liftST $ MArray.newArray bounds '.'
  currSF <- gets currSeaFloor
  liftST $ copyArray currSF lastSF
  moveUntilStable' lastSF
  where
    moveUntilStable' :: STSeaFloor s -> CucumberM s ()
    moveUntilStable' lastSF = do
      moveAll
      currSF <- gets currSeaFloor
      -- Attempt at primitive comparison with memcmp
      -- isSame <- liftST $ compareArrays currSF lastSF
      -- A naive approach with two lists
      -- currElems <- liftST $ MArray.getElems currSF
      -- lastElems <- liftST $ MArray.getElems lastSF
      -- let isSame = currElems == lastElems
      -- Comparing using indexes, but it still builds a list
      bounds <- getBounds
      isSame <- and <$> mapM (\i -> do
        c1 <- liftST $ MArray.readArray currSF i
        c2 <- liftST $ MArray.readArray lastSF i
        return $ c1 == c2) (Ix.range bounds)
      if isSame
        then return ()
        else do
          liftST $ copyArray currSF lastSF
          moveUntilStable' lastSF

-- Solution
--------------------------------------------------

solution :: Solution Input
solution =
  Solution
    { solutionParser = seaFloorParser,
      solutionPart1 = snd . runCucumbers moveUntilStable,
      solutionPart2 = const 0
    }