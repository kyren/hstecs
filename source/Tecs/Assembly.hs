module Tecs.Assembly (
  assembleOperations,
  assemble,
  printOperations
) where

import Numeric
import Data.Char
import Data.Word
import Data.Bits
import qualified Data.Map as Map
import Tecs.Definitions
import Tecs.Parsing

type Symbols = Map.Map String Word16

-- Does not guard against running out of variable or instruction space yet.
computeSymbols :: [Instruction] -> Symbols
computeSymbols instructions = addVariables startingVariableMemory (addLabels 0 predefinedVariables instructions) instructions
  where
    predefinedVariables = Map.fromList [(pVariableName var, pVariableValue var) | var <- [minBound..maxBound]]
    addLabels pos labels (LabelInstruction label : rest) = addLabels pos (Map.insert label pos labels) rest
    addLabels pos labels (_ : rest) = addLabels (pos + 1) labels rest
    addLabels _ labels [] = labels
    addVariables nextMemory symbols (AInstruction (AName name) : rest)
      | Map.member name symbols = addVariables nextMemory symbols rest
      | otherwise = addVariables (nextMemory + 1) (Map.insert name nextMemory symbols) rest
    addVariables nextMemory symbols (_ : rest) = addVariables nextMemory symbols rest
    addVariables _ symbols [] = symbols

-- Uses partial Map.!, relies on computeSymbols containing every symbol
assembleOperations :: [Instruction] -> [Operation]
assembleOperations instructions = go instructions
    where
      symbols = computeSymbols instructions
      go (AInstruction (AName name) : rest) = AOperation (symbols Map.! name) : go rest
      go (AInstruction (AConstant value) : rest) = AOperation value : go rest
      go (LabelInstruction _ : rest) = go rest
      go (CInstruction comp dest jump : rest) = COperation comp dest jump : go rest
      go [] = []

assemble :: String -> Either String [Operation]
assemble content = case parseInstructions content of
  Left err -> Left err
  Right inst -> Right $ assembleOperations inst

operationWord :: Operation -> Word16
operationWord (AOperation val) = 0x7fff .&. val
operationWord (COperation comp dest jump) = 0xe000 .|. bits compBits 7 6 .|. bits destBits 3 3 .|. bits jumpBits 3 0
  where
    compBits = fromIntegral $ compValue comp
    destBits = fromIntegral $ destValue dest
    jumpBits = fromIntegral $ jumpValue jump
    bits val bc = shift (shift 0xffff (bc - 16) .&. val)

padResize :: Int -> a -> [a] -> [a]
padResize targetLength pad input = replicate (targetLength - inputLength) pad ++ drop (inputLength - targetLength) input
  where inputLength = length input

printBinary :: (Integral a, Show a) => Int -> a -> String
printBinary digits num = padResize digits '0' (showIntAtBase 2 intToDigit num "")

printOperations :: [Operation] -> String
printOperations ops = unlines $ map (printBinary 16 . operationWord) ops
