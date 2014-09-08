module Tecs.Definitions (
  AValue(..),
  Comp(..),
  Dest(..),
  Jump(..),
  Instruction(..),
  Operation(..),
  PVariable(..),
  compName,
  compValue,
  destName,
  destValue,
  jumpName,
  jumpValue,
  pVariableName,
  pVariableValue,
  startingVariableMemory,
  maximumVariableMemory,
  maximumAConstant
) where

import Data.Word

data AValue = AName String | AConstant Word16
  deriving Show

data Comp =
  CompZero |
  CompOne |
  CompMinusOne |
  CompD |
  CompA |
  CompNotD |
  CompNotA |
  CompMinusD |
  CompMinusA |
  CompDPlusOne |
  CompAPlusOne |
  CompDMinusOne |
  CompAMinusOne |
  CompDPlusA |
  CompDMinusA |
  CompAMinusD |
  CompDAndA |
  CompDOrA |
  CompM |
  CompNotM |
  CompMinusM |
  CompMPlusOne |
  CompMMinusOne |
  CompDPlusM |
  CompDMinusM |
  CompMMinusD |
  CompDAndM |
  CompDOrM
  deriving (Enum, Bounded, Show)

data Dest =
  DestNULL |
  DestM |
  DestD |
  DestMD |
  DestA |
  DestAM |
  DestAD |
  DestAMD
  deriving (Enum, Bounded, Show)

data Jump =
  JumpNULL |
  JumpGT |
  JumpEQ |
  JumpGE |
  JumpLT |
  JumpNE |
  JumpLE |
  JumpMP
  deriving (Enum, Bounded, Show)

data Instruction =
  AInstruction AValue |
  LabelInstruction String |
  CInstruction Comp Dest Jump
  deriving Show

data PVariable =
  PVariableSP |
  PVariableLCL |
  PVariableARG |
  PVariableTHIS |
  PVariableTHAT |
  PVariableR0 |
  PVariableR1 |
  PVariableR2 |
  PVariableR3 |
  PVariableR4 |
  PVariableR5 |
  PVariableR6 |
  PVariableR7 |
  PVariableR8 |
  PVariableR9 |
  PVariableR10 |
  PVariableR11 |
  PVariableR12 |
  PVariableR13 |
  PVariableR14 |
  PVariableR15 |
  PVariableSCREEN |
  PVariableKBD
  deriving (Enum, Bounded, Show)

data Operation = AOperation Word16 | COperation Word8 Word8 Word8
  deriving Show

compName :: Comp -> String
compName CompZero = "0"
compName CompOne = "1"
compName CompMinusOne = "-1"
compName CompD = "D"
compName CompA = "A"
compName CompNotD = "!D"
compName CompNotA = "!A"
compName CompMinusD = "-D"
compName CompMinusA = "-A"
compName CompDPlusOne = "D+1"
compName CompAPlusOne = "A+1"
compName CompDMinusOne = "D-1"
compName CompAMinusOne = "A-1"
compName CompDPlusA = "D+A"
compName CompDMinusA = "D-A"
compName CompAMinusD = "A-D"
compName CompDAndA = "D&A"
compName CompDOrA = "D|A"
compName CompM = "M"
compName CompNotM = "!M"
compName CompMinusM = "-M"
compName CompMPlusOne = "M+1"
compName CompMMinusOne = "M-1"
compName CompDPlusM = "D+M"
compName CompDMinusM = "D-M"
compName CompMMinusD = "M-D"
compName CompDAndM = "D&M"
compName CompDOrM = "D|M"

compValue :: Comp -> Word8
compValue CompZero = 0x2a
compValue CompOne = 0x3f
compValue CompMinusOne = 0x3a
compValue CompD = 0x0c
compValue CompA = 0x30
compValue CompNotD = 0x0d
compValue CompNotA = 0x31
compValue CompMinusD = 0x0f
compValue CompMinusA = 0x33
compValue CompDPlusOne = 0x1f
compValue CompAPlusOne = 0x37
compValue CompDMinusOne = 0x0e
compValue CompAMinusOne = 0x32
compValue CompDPlusA = 0x02
compValue CompDMinusA = 0x13
compValue CompAMinusD = 0x07
compValue CompDAndA = 0x00
compValue CompDOrA = 0x15
compValue CompM = 0x70
compValue CompNotM = 0x71
compValue CompMinusM = 0x73
compValue CompMPlusOne = 0x77
compValue CompMMinusOne = 0x72
compValue CompDPlusM = 0x42
compValue CompDMinusM = 0x43
compValue CompMMinusD = 0x47
compValue CompDAndM = 0x40
compValue CompDOrM = 0x55

destName :: Dest -> String
destName DestNULL = "NULL"
destName DestM = "M"
destName DestD = "D"
destName DestMD = "MD"
destName DestA = "A"
destName DestAM = "AM"
destName DestAD = "AD"
destName DestAMD = "AMD"

destValue :: Dest -> Word8
destValue DestNULL = 0x0
destValue DestM = 0x1
destValue DestD = 0x2
destValue DestMD = 0x3
destValue DestA = 0x4
destValue DestAM = 0x5
destValue DestAD = 0x6
destValue DestAMD = 0x7

jumpName :: Jump -> String
jumpName JumpNULL = "NULL"
jumpName JumpGT = "JGT"
jumpName JumpEQ = "JEQ"
jumpName JumpGE = "JGE"
jumpName JumpLT = "JLT"
jumpName JumpNE = "JNE"
jumpName JumpLE = "JLE"
jumpName JumpMP = "JMP"

jumpValue :: Jump -> Word8
jumpValue JumpNULL = 0x0
jumpValue JumpGT = 0x1
jumpValue JumpEQ = 0x2
jumpValue JumpGE = 0x3
jumpValue JumpLT = 0x4
jumpValue JumpNE = 0x5
jumpValue JumpLE = 0x6
jumpValue JumpMP = 0x7

pVariableName :: PVariable -> String
pVariableName PVariableSP = "SP"
pVariableName PVariableLCL = "LCL"
pVariableName PVariableARG = "ARG"
pVariableName PVariableTHIS = "THIS"
pVariableName PVariableTHAT = "THAT"
pVariableName PVariableR0 = "R0"
pVariableName PVariableR1 = "R1"
pVariableName PVariableR2 = "R2"
pVariableName PVariableR3 = "R3"
pVariableName PVariableR4 = "R4"
pVariableName PVariableR5 = "R5"
pVariableName PVariableR6 = "R6"
pVariableName PVariableR7 = "R7"
pVariableName PVariableR8 = "R8"
pVariableName PVariableR9 = "R9"
pVariableName PVariableR10 = "R10"
pVariableName PVariableR11 = "R11"
pVariableName PVariableR12 = "R12"
pVariableName PVariableR13 = "R13"
pVariableName PVariableR14 = "R14"
pVariableName PVariableR15 = "R15"
pVariableName PVariableSCREEN = "SCREEN"
pVariableName PVariableKBD = "KBD"

pVariableValue :: PVariable -> Word16
pVariableValue PVariableSP = 0
pVariableValue PVariableLCL = 1
pVariableValue PVariableARG = 2
pVariableValue PVariableTHIS = 3
pVariableValue PVariableTHAT = 4
pVariableValue PVariableR0 = 0
pVariableValue PVariableR1 = 1
pVariableValue PVariableR2 = 2
pVariableValue PVariableR3 = 3
pVariableValue PVariableR4 = 4
pVariableValue PVariableR5 = 5
pVariableValue PVariableR6 = 6
pVariableValue PVariableR7 = 7
pVariableValue PVariableR8 = 8
pVariableValue PVariableR9 = 9
pVariableValue PVariableR10 = 10
pVariableValue PVariableR11 = 11
pVariableValue PVariableR12 = 12
pVariableValue PVariableR13 = 13
pVariableValue PVariableR14 = 14
pVariableValue PVariableR15 = 15
pVariableValue PVariableSCREEN = 16384
pVariableValue PVariableKBD = 24576

startingVariableMemory :: Word16
startingVariableMemory = 16

maximumVariableMemory :: Word16
maximumVariableMemory = 16383

maximumAConstant :: Word16
maximumAConstant = 32767
