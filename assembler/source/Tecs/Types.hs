module Tecs.Types (
  AValue(..),
  Instruction(..),
  Operation(..),
  ) where

import Data.Int

data AValue = AName String | AValue Int16
data Instruction =
  AInstruction AValue |
  LabelInstruction String |
  CInstruction String String String

data Operation = AOperation Int16 | COperation Int8 Int8 Int8
