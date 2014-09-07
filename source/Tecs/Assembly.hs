module Tecs.Assembly (
  assembleOperations,
  assemble,
  printOperations
) where

import Data.Char
import Numeric
import Control.Applicative
import qualified Data.Map as Map
import Tecs.Definitions
import Tecs.Types
import Tecs.Parsing

-- Does not guard against running out of symbol space yet
computeSymbols :: [Instruction] -> Symbols
computeSymbols instructions = addVariables startingVariableMemory (addLabels 0 predefinedVariables instructions) instructions
  where
    addLabels pos labels (LabelInstruction label : rest) = addLabels pos (Map.insert label pos labels) rest
    addLabels pos labels (_ : rest) = addLabels (pos + 1) labels rest
    addLabels _ labels [] = labels
    addVariables nextMemory symbols (AInstruction (AName name) : rest)
      | Map.member name symbols = addVariables nextMemory symbols rest
      | otherwise = addVariables (nextMemory + 1) (Map.insert name nextMemory symbols) rest
    addVariables nextMemory symbols (_ : rest) = addVariables nextMemory symbols rest
    addVariables _ symbols [] = symbols

assembleOperations :: [Instruction] -> Either String [Operation]
assembleOperations instructions = sequence $ go instructions
  where
    symbols = computeSymbols instructions
    mapLookup name m mapType = case Map.lookup name m of
      Nothing -> Left $ "No such symbol " ++ name ++ " found in " ++ mapType ++ " table"
      Just r -> Right r
    go (AInstruction (AName name) : rest) = (AOperation <$> mapLookup name symbols "symbols") : go rest
    go (AInstruction (AConstant value) : rest) = Right (AOperation value) : go rest
    go (LabelInstruction _ : rest) = go rest
    go (CInstruction comp dest jump : rest) = (COperation <$>
      mapLookup comp compMap "compute" <*>
      mapLookup dest destMap "destination" <*>
      mapLookup jump jumpMap "jump") : go rest
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

