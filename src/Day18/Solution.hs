module Solution where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Attoparsec.Text as P
import Exercise

data SFNumber
  = Pair SFNumber SFNumber
  | Regular Int
  deriving (Eq)

instance Show SFNumber where
  show (Regular n) = show n
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

-- Parsing
--------------------------------------------------

type Input = [SFNumber]

regularParser = Regular <$> P.decimal

pairParser = do
  P.char '['
  a <- numberParser
  P.char ','
  b <- numberParser
  P.char ']'
  return $ Pair a b

numberParser = regularParser <|> pairParser

inputParser =
  pairParser `P.sepBy` P.endOfLine

-- Mechanics
--------------------------------------------------

data NumberContext
  = Root
  | L SFNumber NumberContext
  | R SFNumber NumberContext
  deriving (Show)

type NumberZipper = (SFNumber, NumberContext)

toZipper :: SFNumber -> NumberZipper
toZipper n = (n, Root)

unzipper :: NumberZipper -> SFNumber
unzipper (n, Root) = n
unzipper (n, L r ctx) = unzipper (Pair n r, ctx)
unzipper (n, R l ctx) = unzipper (Pair l n, ctx)

newtype Zipper a = Zip
  { runZipper :: StateT NumberZipper (Except String) a
  }
  deriving (Functor, Applicative, Alternative, Monad, MonadError String, MonadState NumberZipper)

execZipper :: Zipper a -> NumberZipper -> Either String (a, NumberZipper)
execZipper m = runExcept . runStateT (runZipper m)

topmost :: Zipper ()
topmost = do
  (n, ctx) <- get
  case ctx of
    Root -> return ()
    (L r ctx) -> put (Pair n r, ctx) >> topmost
    (R l ctx) -> put (Pair l n, ctx) >> topmost

leftmost :: Zipper ()
leftmost = do
  (n, ctx) <- get
  case n of
    r@(Regular _) -> pure ()
    Pair l r -> do
      put (l, L r ctx)
      leftmost

rightmost :: Zipper ()
rightmost = do
  (n, ctx) <- get
  case n of
    r@(Regular _) -> pure ()
    Pair l r -> do
      put (r, R l ctx)
      rightmost

nextRegularRight :: Zipper ()
nextRegularRight = do
  (n, ctx) <- get
  case ctx of
    Root -> throwError "Cannot move right from root"
    (L r ctx) -> do
      put (r, R n ctx)
      leftmost
    (R l ctx) -> do
      put (Pair l n, ctx)
      nextRegularRight

nextRegularLeft :: Zipper ()
nextRegularLeft = do
  (n, ctx) <- get
  case ctx of
    Root -> throwError "Cannot move left from root"
    (L r ctx) -> do
      put (Pair n r, ctx)
      nextRegularLeft
    (R l ctx) -> do
      put (l, L n ctx)
      rightmost

moveUp :: Zipper ()
moveUp = do
  (n, ctx) <- get
  case ctx of
    Root -> throwError "Cannot move up from root"
    (L r ctx) ->
      put (Pair n r, ctx)
    (R l ctx) ->
      put (Pair l n, ctx)

getFocus :: Zipper SFNumber
getFocus =
  gets fst

depth :: Zipper Int
depth =
  gets $ contextLength . snd
  where
    contextLength Root = 0
    contextLength (L r ctx) = 1 + contextLength ctx
    contextLength (R l ctx) = 1 + contextLength ctx

update :: (SFNumber -> SFNumber) -> Zipper ()
update f = do
  (n, ctx) <- get
  put (f n, ctx)

updateInt :: (Int -> Int) -> Zipper ()
updateInt f = do
  (sfn, ctx) <- get
  case sfn of
    Regular n -> put (Regular $ f n, ctx)
    _ -> throwError $ "Cannot update non-regular number: " ++ show sfn

explode :: Zipper Bool
explode = do
  topmost
  leftmost
  explodeFirst
  where
    explodeFirst = do
      d <- depth
      if d > 4
        then do
          moveUp
          focus <- getFocus
          case focus of
            Pair (Regular l) (Regular r) -> do
              update (const (Regular 0))
              (nextRegularLeft >> updateInt (+ l) >> nextRegularRight) <|> pure ()
              (nextRegularRight >> updateInt (+ r)) <|> pure ()
              return True
            _ -> throwError $ "Cannot explode nested pair: " ++ show focus
        else do
          (nextRegularRight >> explodeFirst) <|> return False

split :: Zipper Bool
split = do
  topmost
  leftmost
  splitFirst
  where
    splitFirst = do
      n <- getFocus
      case n of
        (Pair _ _) ->
          -- Not sure how we got here...
          throwError $ "Running splitFirst on non-number: " ++ show n
        (Regular n) ->
          if n >= 10
            then do
              let left = n `div` 2
                  right = ceiling . (/ 2) . fromIntegral $ n
              update (const (Pair (Regular left) (Regular right)))
              return True
            else (nextRegularRight >> splitFirst) <|> return False

reduce :: Zipper ()
reduce = do
  exploded <- explode
  if exploded
    then reduce
    else do
      wasSplit <- split
      if wasSplit
        then reduce
        else pure ()

add :: SFNumber -> SFNumber -> SFNumber
add a b =
  case execZipper reduce . toZipper $ Pair a b of
    Left err -> error err
    Right (_, n) -> unzipper n

addAll :: [SFNumber] -> SFNumber
addAll = foldl1 add

magnitude :: SFNumber -> Int
magnitude (Regular n) = n
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = magnitude . addAll,
      solutionPart2 = \input ->
        maximum [magnitude (add a b) | a <- input, b <- input]
    }