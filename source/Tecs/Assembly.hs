module Tecs.Assembly (
  assembleOperations,
  assemble,
  printOperations
) where

import Data.Char
import Data.Word
import Numeric
import Control.Applicative
import qualified Data.Map as Map
import Tecs.Definitions
import Tecs.Parsing

type Symbols = Map.Map String Word16

-- None of the assembly functions guard against running out of variable or
-- instruction space yet.
computeSymbols :: [Instruction] -> Either String Symbols
computeSymbols instructions = Right $ addVariables startingVariableMemory (addLabels 0 predefinedVariables instructions) instructions
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

assembleOperations :: [Instruction] -> Either String [Operation]
assembleOperations instructions = case computeSymbols instructions of
  Left err -> Left err
  Right symbols -> sequence $ go instructions
    where
      symbolLookup name = case Map.lookup name symbols of
        Nothing -> Left $ "No such symbol " ++ name ++ " found in symbols table"
        Just r -> Right r
      go (AInstruction (AName name) : rest) = (AOperation <$> symbolLookup name) : go rest
      go (AInstruction (AConstant value) : rest) = Right (AOperation value) : go rest
      go (LabelInstruction _ : rest) = go rest
      go (CInstruction comp dest jump : rest) = Right (COperation (compValue comp) (destValue dest) (jumpValue jump)) : go rest
      go [] = []

assemble :: String -> Either String [Operation]
assemble content = parseInstructions content >>= assembleOperations

padResize :: Int -> a -> [a] -> [a]
padResize targetLength pad input = replicate (targetLength - inputLength) pad ++ drop (inputLength - targetLength) input
  where inputLength = length input

printBinary :: (Integral a, Show a) => Int -> a -> String
printBinary digits num = padResize digits '0' (showIntAtBase 2 intToDigit num "")

printOperations :: [Operation] -> String
printOperations ops = unlines $ map printOperationBinary ops
  where
    printOperationBinary (AOperation num) = '0' : printBinary 15 num 
    printOperationBinary (COperation comp dest jump) = "111" ++ printBinary 7 comp ++ printBinary 3 dest ++ printBinary 3 jump

