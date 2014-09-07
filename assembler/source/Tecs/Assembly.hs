module Tecs.Assembly (
  assemble,
  printOperations
  ) where

import Data.Int
import Control.Applicative
import qualified Data.Map as Map
import Tecs.Utility
import Tecs.Definitions
import Tecs.Types
import Tecs.Parsing

type Symbols = Map.Map String Int16

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

makeOperations :: [Instruction] -> Maybe [Operation]
makeOperations instructions = sequence $ go (computeSymbols instructions) instructions
  where
    go symbols (AInstruction (AName name) : rest) = (AOperation <$> Map.lookup name symbols) : go symbols rest
    go symbols (AInstruction (AValue value) : rest) = Just (AOperation value) : go symbols rest
    go symbols (LabelInstruction _ : rest) = go symbols rest
    go symbols (CInstruction comp dest jump : rest) = (COperation <$> Map.lookup comp compMap <*> Map.lookup dest destMap <*> Map.lookup jump jumpMap) : go symbols rest
    go _ [] = []

assemble :: String -> Maybe [Operation]
assemble content = parseInstructions content >>= makeOperations

printOperations :: [Operation] -> String
printOperations ops = unlines $ map printOperationBinary ops
  where
    printOperationBinary (AOperation num) = '0' : printBinary 15 num 
    printOperationBinary (COperation comp dest jump) = "111" ++ printBinary 7 comp ++ printBinary 3 dest ++ printBinary 3 jump

