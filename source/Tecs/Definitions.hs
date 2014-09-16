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

data Operation =
  AOperation Word16 |
  COperation Comp Dest Jump
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

compDesc :: Comp -> (String, Word8)
compDesc CompZero = ("0", 0x2a)
compDesc CompOne = ("1", 0x3f)
compDesc CompMinusOne = ("-1", 0x3a)
compDesc CompD = ("D", 0x0c)
compDesc CompA = ("A", 0x30)
compDesc CompNotD = ("!D", 0x0d)
compDesc CompNotA = ("!A", 0x31)
compDesc CompMinusD = ("-D", 0x0f)
compDesc CompMinusA = ("-A", 0x33)
compDesc CompDPlusOne = ("D+1", 0x1f)
compDesc CompAPlusOne = ("A+1", 0x37)
compDesc CompDMinusOne = ("D-1", 0x0e)
compDesc CompAMinusOne = ("A-1", 0x32)
compDesc CompDPlusA = ("D+A", 0x02)
compDesc CompDMinusA = ("D-A", 0x13)
compDesc CompAMinusD = ("A-D", 0x07)
compDesc CompDAndA = ("D&A", 0x00)
compDesc CompDOrA = ("D|A", 0x15)
compDesc CompM = ("M", 0x70)
compDesc CompNotM = ("!M", 0x71)
compDesc CompMinusM = ("-M", 0x73)
compDesc CompMPlusOne = ("M+1", 0x77)
compDesc CompMMinusOne = ("M-1", 0x72)
compDesc CompDPlusM = ("D+M", 0x42)
compDesc CompDMinusM = ("D-M", 0x43)
compDesc CompMMinusD = ("M-D", 0x47)
compDesc CompDAndM = ("D&M", 0x40)
compDesc CompDOrM = ("D|M", 0x55)

compName :: Comp -> String
compName = fst . compDesc

compValue :: Comp -> Word8
compValue = snd . compDesc

destDesc :: Dest -> (String, Word8)
destDesc DestNULL = ("NULL", 0x0)
destDesc DestM = ("M", 0x1)
destDesc DestD = ("D", 0x2)
destDesc DestMD = ("MD", 0x3)
destDesc DestA = ("A", 0x4)
destDesc DestAM = ("AM", 0x5)
destDesc DestAD = ("AD", 0x6)
destDesc DestAMD = ("AMD", 0x7)

destName :: Dest -> String
destName = fst . destDesc

destValue :: Dest -> Word8
destValue = snd . destDesc

jumpDesc :: Jump -> (String, Word8)
jumpDesc JumpNULL = ("NULL", 0x0)
jumpDesc JumpGT = ("JGT", 0x1)
jumpDesc JumpEQ = ("JEQ", 0x2)
jumpDesc JumpGE = ("JGE", 0x3)
jumpDesc JumpLT = ("JLT", 0x4)
jumpDesc JumpNE = ("JNE", 0x5)
jumpDesc JumpLE = ("JLE", 0x6)
jumpDesc JumpMP = ("JMP", 0x7)

jumpName :: Jump -> String
jumpName = fst . jumpDesc

jumpValue :: Jump -> Word8
jumpValue = snd . jumpDesc

pVariableDesc :: PVariable -> (String, Word16)
pVariableDesc PVariableSP = ("SP", 0x0)
pVariableDesc PVariableLCL = ("LCL", 0x1)
pVariableDesc PVariableARG = ("ARG", 0x2)
pVariableDesc PVariableTHIS = ("THIS", 0x3)
pVariableDesc PVariableTHAT = ("THAT", 0x4)
pVariableDesc PVariableR0 = ("R0", 0x0)
pVariableDesc PVariableR1 = ("R1", 0x1)
pVariableDesc PVariableR2 = ("R2", 0x2)
pVariableDesc PVariableR3 = ("R3", 0x3)
pVariableDesc PVariableR4 = ("R4", 0x4)
pVariableDesc PVariableR5 = ("R5", 0x5)
pVariableDesc PVariableR6 = ("R6", 0x6)
pVariableDesc PVariableR7 = ("R7", 0x7)
pVariableDesc PVariableR8 = ("R8", 0x8)
pVariableDesc PVariableR9 = ("R9", 0x9)
pVariableDesc PVariableR10 = ("R10", 0xa)
pVariableDesc PVariableR11 = ("R11", 0xb)
pVariableDesc PVariableR12 = ("R12", 0xc)
pVariableDesc PVariableR13 = ("R13", 0xd)
pVariableDesc PVariableR14 = ("R14", 0xe)
pVariableDesc PVariableR15 = ("R15", 0xf)
pVariableDesc PVariableSCREEN = ("SCREEN", 0x4000)
pVariableDesc PVariableKBD = ("KBD", 0x6000)

pVariableName :: PVariable -> String
pVariableName = fst . pVariableDesc

pVariableValue :: PVariable -> Word16
pVariableValue = snd . pVariableDesc

startingVariableMemory :: Word16
startingVariableMemory = 16

maximumVariableMemory :: Word16
maximumVariableMemory = 0x3fff

maximumAConstant :: Word16
maximumAConstant = 0x7fff
