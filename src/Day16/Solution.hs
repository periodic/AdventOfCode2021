module Solution where

import Control.Monad
import Control.Monad.State
import qualified Data.Attoparsec.Text as P
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Exercise
import Text.Printf

-- Hex/Binary utils 
--------------------------------------------------

newtype HexadecimalString = HexadecimalString {hexData :: Text}
  deriving (Eq, Show)

newtype BinaryString = BinaryString {binData :: Text}
  deriving (Eq, Show)

hexToBin :: HexadecimalString -> BinaryString
hexToBin =
  BinaryString
    . Text.concatMap
      ( \case
          '0' -> "0000"
          '1' -> "0001"
          '2' -> "0010"
          '3' -> "0011"
          '4' -> "0100"
          '5' -> "0101"
          '6' -> "0110"
          '7' -> "0111"
          '8' -> "1000"
          '9' -> "1001"
          'A' -> "1010"
          'B' -> "1011"
          'C' -> "1100"
          'D' -> "1101"
          'E' -> "1110"
          'F' -> "1111"
      )
    . hexData

readBin :: BinaryString -> Int
readBin =
  Text.foldl go 0 . binData
  where
    go :: Int -> Char -> Int
    go acc '0' = acc * 2
    go acc '1' = acc * 2 + 1

-- Parsing 
--------------------------------------------------

type Input = HexadecimalString

inputParser :: P.Parser Input
inputParser =
  HexadecimalString <$> P.takeWhile1 (P.inClass "0-9A-F")

-- Packets
--------------------------------------------------

data PacketData
  = PacketLiteral {literalData :: Int}
  | PacketOperator {operatorTypeId :: Int, operatorPackets :: [Packet]}
  deriving (Eq)

instance Show PacketData where
  show (PacketLiteral n) = show n
  show (PacketOperator n packets) =
    "(" ++ show n ++ ", " ++ List.intercalate ", " (map show packets) ++ ")"

data Packet = Packet
  { packetVersion :: Int,
    packetData :: PacketData
  }
  deriving (Eq)

instance Show Packet where
  show Packet {..} =
    "<[" ++ show packetVersion ++ "] " ++ show packetData ++ ">"

-- Packet Parsing
--------------------------------------------------

data PacketParseState = PacketParseState
  { packetParsePos :: Int,
    packetParseBits :: Text
  }
  deriving (Show)

type Binary = State PacketParseState

runBinary :: BinaryString -> Binary a -> a
runBinary (BinaryString bits) =
  flip evalState $ PacketParseState 0 bits

takeBits :: Int -> Binary BinaryString
takeBits n = do
  PacketParseState {..} <- get
  if n > Text.length packetParseBits
    then error $ printf "Not enough bits (%d) to take (%d)" (Text.length packetParseBits) n
    else do
      let (left, right) = Text.splitAt n packetParseBits
      put $
        PacketParseState
          { packetParsePos = packetParsePos + n,
            packetParseBits = right
          }
      return $ BinaryString left

takeInt :: Int -> Binary Int
takeInt n = readBin <$> takeBits n

packetFromHex :: HexadecimalString -> Packet
packetFromHex hexPacket =
  runBinary (hexToBin hexPacket) decodePacket

decodePacket :: Binary Packet
decodePacket = do
  version <- takeInt 3
  operatorTypeId <- takeInt 3
  Packet version <$> case operatorTypeId of
    4 -> decodeLiteral
    tid -> decodeOperator tid

decodeLiteral :: Binary PacketData
decodeLiteral = do
  bits <- readLiteralBits
  return . PacketLiteral . readBin . BinaryString . Text.concat $ bits

readLiteralBits :: Binary [Text]
readLiteralBits = do
  (BinaryString bits) <- takeBits 5
  if Text.head bits == '1'
    then (Text.drop 1 bits :) <$> readLiteralBits
    else return [Text.drop 1 bits]

decodeOperator :: Int -> Binary PacketData
decodeOperator tid = do
  lengthTypeId <- takeInt 1
  case lengthTypeId of
    0 -> do
      bitLength <- takeInt 15
      currPos <- gets packetParsePos
      PacketOperator tid <$> decodePacketsUntilBit (currPos + bitLength)
    1 -> do
      bitLength <- takeInt 11
      PacketOperator tid <$> replicateM bitLength decodePacket
  where
    decodePacketsUntilBit :: Int -> Binary [Packet]
    decodePacketsUntilBit endPos = do
      currPos <- gets packetParsePos
      if currPos < endPos
        then do
          packet <- decodePacket
          (packet :) <$> decodePacketsUntilBit endPos
        else return []

-- Part 1
--------------------------------------------------

sumVersions :: Packet -> Int
sumVersions (Packet version packetData) =
  version + sumVersionsData packetData
  where
    sumVersionsData :: PacketData -> Int
    sumVersionsData (PacketLiteral literal) = 0
    sumVersionsData (PacketOperator _ packets) = sum (map sumVersions packets)

-- Part 2
--------------------------------------------------

evalPacket :: Packet -> Int
evalPacket (Packet _ (PacketLiteral literal)) = literal
evalPacket (Packet _ (PacketOperator op packets)) =
  case op of
    0 -> sum (map evalPacket packets)
    1 -> product (map evalPacket packets)
    2 -> minimum (map evalPacket packets)
    3 -> maximum (map evalPacket packets)
    5 ->
      let (a : b : _) = map evalPacket packets
       in if a > b then 1 else 0
    6 ->
      let (a : b : _) = map evalPacket packets
       in if a < b then 1 else 0
    7 ->
      let (a : b : _) = map evalPacket packets
       in if a == b then 1 else 0

solution :: Solution Input
solution =
  Solution
    { solutionParser = inputParser,
      solutionPart1 = sumVersions . packetFromHex,
      solutionPart2 = evalPacket . packetFromHex
    }