module ParserHelpers where

import qualified Data.Attoparsec.Text as P

parseGrid :: P.Parser [a] -> P.Parser [((Int, Int), a)]
parseGrid row =
  listToMap
    <$> row `P.sepBy1` P.endOfLine
  where
    listToMap =
        concatMap
          ( \(y, row) ->
              map (\(x, val) -> ((x, y), val)) . indexed $ row
          )
        . indexed

indexed :: [a] -> [(Int, a)]
indexed = zip [1 ..]