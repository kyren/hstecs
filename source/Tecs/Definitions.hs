module Tecs.Definitions (
  compMap,
  destMap,
  jumpMap,
  predefinedVariables,
  startingVariableMemory
) where

import Data.Int
import qualified Data.Map as Map

compMap :: Map.Map String Int8
compMap = Map.fromList [
    ("0", 0x2a),
    ("1", 0x3f),
    ("-1", 0x3a),
    ("D", 0x0c),
    ("A", 0x30),
    ("!D", 0x0d),
    ("!A", 0x31),
    ("-D", 0x0f),
    ("-A", 0x33),
    ("D+1", 0x1f),
    ("A+1", 0x37),
    ("D-1", 0x0e),
    ("A-1", 0x32),
    ("D+A", 0x02),
    ("D-A", 0x13),
    ("A-D", 0x07),
    ("D&A", 0x00),
    ("D|A", 0x15),
    ("M", 0x70),
    ("!M", 0x71),
    ("-M", 0x73),
    ("M+1", 0x77),
    ("M-1", 0x72),
    ("D+M", 0x42),
    ("D-M", 0x43),
    ("M-D", 0x47),
    ("D&M", 0x40),
    ("D|M", 0x55)
  ]

destMap :: Map.Map String Int8
destMap = Map.fromList [
    ("NULL", 0x0),
    ("M", 0x1),
    ("D", 0x2),
    ("MD", 0x3),
    ("A", 0x4),
    ("AM", 0x5),
    ("AD", 0x6),
    ("AMD", 0x7)
  ]

jumpMap :: Map.Map String Int8
jumpMap = Map.fromList [
    ("NULL", 0x0),
    ("JGT", 0x1),
    ("JEQ", 0x2),
    ("JGE", 0x3),
    ("JLT", 0x4),
    ("JNE", 0x5),
    ("JLE", 0x6),
    ("JMP", 0x7)
  ]

predefinedVariables :: Map.Map String Int16
predefinedVariables = Map.fromList [
    ("SP", 0),
    ("LCL", 1),
    ("ARG", 2),
    ("THIS", 3),
    ("THAT", 4),
    ("R0", 0),
    ("R1", 1),
    ("R2", 2),
    ("R3", 3),
    ("R4", 4),
    ("R5", 5),
    ("R6", 6),
    ("R7", 7),
    ("R8", 8),
    ("R9", 9),
    ("R10", 10),
    ("R11", 11),
    ("R12", 12),
    ("R13", 13),
    ("R14", 14),
    ("R15", 15),
    ("SCREEN", 16384),
    ("KBD", 24576)
  ]

startingVariableMemory :: Int16
startingVariableMemory = 16
