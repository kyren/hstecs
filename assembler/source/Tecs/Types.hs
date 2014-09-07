module Tecs.Types (
  AValue(..),
  Instruction(..),
  Symbols,
  Operation(..)
) where

import Data.Int
import qualified Data.Map as Map

data AValue = AName String | AConstant Int16
  deriving Show

data Instruction =
  AInstruction AValue |
  LabelInstruction String |
  CInstruction String String String
  deriving Show

type Symbols = Map.Map String Int16

data Operation = AOperation Int16 | COperation Int8 Int8 Int8
  deriving Show
