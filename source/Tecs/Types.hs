module Tecs.Types (
  AValue(..),
  Instruction(..),
  Symbols,
  Operation(..)
) where

import Data.Word
import qualified Data.Map as Map

data AValue = AName String | AConstant Word16
  deriving Show

data Instruction =
  AInstruction AValue |
  LabelInstruction String |
  CInstruction String String String
  deriving Show

type Symbols = Map.Map String Word16

data Operation = AOperation Word16 | COperation Word8 Word8 Word8
  deriving Show
