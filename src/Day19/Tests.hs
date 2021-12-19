module Tests where

import Test.Hspec
import Linear.V3
import qualified Data.List as List
import qualified Data.Set as Set

import Solution

noR = NilR
zcw = R ZCW
zccw = R ZCCW
xcw = R XCW
xccw = R XCCW
ycw = R YCW
yccw = R YCCW

fromList :: [Coord] -> Scanner
fromList =
  scanner . ScannerReadings 0

v0 = V3 0 0 0
vx = V3 1 0 0
vy = V3 0 1 0
vz = V3 0 0 1

testPoints =
  [ V3 x (2*y) (3*z) | x <- [0..4], y <- [0..4], z <- [0..4] ]

uniqueIds =
  zipWith (\i s -> s { scannerId = i }) [0..]

tests = hspec $ do
  describe "scanner rotation" $ do
    it "should rotate X CCW" $ getPoints (rotate XCCW (fromList [vx, vy, vz])) `shouldBe` Set.fromList [vx, vz, -vy]
    it "should rotate X CW" $  getPoints (rotate XCW  (fromList [vx, vy, vz])) `shouldBe` Set.fromList [vx, -vz, vy]
    it "should rotate Y CCW" $ getPoints (rotate YCCW (fromList [vx, vy, vz])) `shouldBe` Set.fromList [-vz, vy, vx]
    it "should rotate Y CW" $  getPoints (rotate YCW  (fromList [vx, vy, vz])) `shouldBe` Set.fromList [vz, vy, -vx]
    it "should rotate Z CCW" $ getPoints (rotate ZCCW (fromList [vx, vy, vz])) `shouldBe` Set.fromList [vy, -vx, vz]
    it "should rotate Z CW" $  getPoints (rotate ZCW  (fromList [vx, vy, vz])) `shouldBe` Set.fromList [-vy, vx, vz]
  describe "findOverlap" $ do
    it "should overlap with itself" $ do
      let scanner = fromList . take 12 $ testPoints
      findOverlap scanner scanner `shouldBe` Just v0
    it "should overlap when translated" $ do
      let scanner1 = fromList . take 12 $ testPoints
          scanner2 = fromList . map (+ vx) . take 12 $ testPoints
      findOverlap scanner1 scanner2 `shouldBe` Just (-vx)
    it "should overlap if 12 points overlap" $ do
      let scanner1 = fromList . (V3 100 0 0 :) . take 12 $ testPoints
          scanner2 = fromList . (V3 200 0 0 :) . take 12 $ testPoints
      findOverlap scanner1 scanner2 `shouldBe` Just v0
    it "should not overlap if rotated" $ do
      let scanner1 = fromList . take 12 $ testPoints
          scanner2 = rotate XCCW . fromList . map (+ vx) . take 12 $ testPoints
      findOverlap scanner1 scanner2 `shouldBe` Nothing
  describe "orient" $ do
    it "should orient to itself" $ do
      let points = take 12 testPoints
          scanner = fromList points
      fmap getPoints (orient scanner scanner) `shouldBe` (Just . getPoints $ scanner)
    it "should combine with itself when rotated" $ do
      let scanner1 = fromList . take 12 $ testPoints
          scanner2 = rotate XCCW scanner1
      fmap getPoints (orient scanner1 scanner2) `shouldBe` (Just . getPoints $ scanner1)
    it "should combine with itself when translated" $ do
      let scanner1 = fromList . take 12 $ testPoints
          scanner2 = translate vx scanner1
      fmap getPoints (orient scanner1 scanner2) `shouldBe` (Just . getPoints $ scanner1)
  describe "orientAll" $ do
    it "should orient multiple copies of the same one" $ do
      let points = take 12 testPoints
          scanner = fromList points
          scanners = uniqueIds $ zipWith (\i s -> s { scannerId = i }) [0..] $ replicate 10 scanner
      orientAll scanners `shouldBe` Set.fromList scanners
    it "should combine different offsets" $ do
      let 
          offsets = [v0, vx, vy, vz]
          scanners = uniqueIds $ map (\offset -> fromList . map (+offset) . take 12 $ testPoints) offsets
      orientAll scanners `shouldBe` Set.fromList scanners
    it "should combine different rotations" $ do
      let 
          scanners = uniqueIds $ map (\r -> setRotation r . fromList . take 12 $ testPoints) allOrientations
      orientAll scanners `shouldBe` Set.fromList scanners

