module Tests where

import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import qualified Data.Text as Text
import Solution
import Test.Hspec

parseAndExec :: Zipper a -> Text -> Either String (a, Text)
parseAndExec action input = do
  n <- P.parseOnly pairParser input
  explodedZipper <- execZipper action . toZipper $ n
  pure . resultToText $ explodedZipper
  where
    resultToText (a, z) = (a, Text.pack . show . unzipper $ z)

sfn :: Text -> SFNumber
sfn input =
  case P.parseOnly pairParser input of
    Left err -> error err
    Right n -> n

main = hspec $ do
  describe "explode" $ do
    it "should not explode insufficient nesting" $ do
      parseAndExec explode "[[[[9,8],1],2],3]" `shouldBe` Right (False, "[[[[9,8],1],2],3]")
    it "works on the leftmost pair" $ do
      parseAndExec explode "[[[[[9,8],1],2],3],4]" `shouldBe` Right (True, "[[[[0,9],2],3],4]")
    it "works on the rightmost pair" $ do
      parseAndExec explode "[7,[6,[5,[4,[3,2]]]]]" `shouldBe` Right (True, "[7,[6,[5,[7,0]]]]")
    it "goes up levels" $ do
      parseAndExec explode "[[6,[5,[4,[3,2]]]],1]" `shouldBe` Right (True, "[[6,[5,[7,0]]],3]")
    it "goes up and down levels" $ do
      parseAndExec explode "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" `shouldBe` Right (True, "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
  describe "split" $ do
    it "does not split 9" $ do
      parseAndExec split "[0,9]" `shouldBe` Right (False, "[0,9]")
    it "splits 10" $ do
      parseAndExec split "[0,10]" `shouldBe` Right (True, "[0,[5,5]]")
    it "splits even numbers evenly" $ do
      parseAndExec split "[0,18]" `shouldBe` Right (True, "[0,[9,9]]")
    it "splits odd numbers unevenly" $ do
      parseAndExec split "[0,17]" `shouldBe` Right (True, "[0,[8,9]]")
    it "splits only the first number" $ do
      parseAndExec split "[[[[0,7],4],[15,[0,13]]],[1,1]]" `shouldBe` Right (True, "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
  describe "reduce" $ do
    it "splits and explodes" $ do
      parseAndExec reduce "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" `shouldBe` Right ((), "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  describe "add" $ do
    it "adds and reduces" $ do
      add (sfn "[[[[4,3],4],4],[7,[[8,4],9]]]") (sfn "[1,1]") `shouldBe` sfn "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    it "has examples" $ do
      foldl1 add (map sfn ["[1,1]", "[2,2]", "[3,3]", "[4,4]"]) `shouldBe` sfn "[[[[1,1],[2,2]],[3,3]],[4,4]]"
      foldl1 add (map sfn ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]"]) `shouldBe` sfn "[[[[3,0],[5,3]],[4,4]],[5,5]]"
      foldl1 add (map sfn ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"]) `shouldBe` sfn "[[[[5,0],[7,4]],[5,5]],[6,6]]"
      foldl1
        add
        ( map
            sfn
            [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
              "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
              "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
              "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
              "[7,[5,[[3,8],[1,4]]]]",
              "[[2,[2,2]],[8,[8,1]]]",
              "[2,9]",
              "[1,[[[9,3],9],[[9,0],[0,7]]]]",
              "[[[5,[7,4]],7],1]",
              "[[[[4,2],2],6],[8,7]]"
            ]
        )
        `shouldBe` sfn "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
  describe "magnitude" $ do
    it "is the value of a regular number" $ do
      magnitude (Regular 5) `shouldBe` 5
    it "is 3*left + 2*right" $ do
      magnitude (sfn "[9,1]") `shouldBe` 29
    it "is recursive" $ do
      magnitude (sfn "[[1,2],[[3,4],5]]") `shouldBe` 143
      magnitude (sfn "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") `shouldBe` 1384
      magnitude (sfn "[[[[1,1],[2,2]],[3,3]],[4,4]]") `shouldBe` 445
      magnitude (sfn "[[[[3,0],[5,3]],[4,4]],[5,5]]") `shouldBe` 791
      magnitude (sfn "[[[[5,0],[7,4]],[5,5]],[6,6]]") `shouldBe` 1137
      magnitude (sfn "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") `shouldBe` 3488
