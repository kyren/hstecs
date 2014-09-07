module Tecs.Utility (
  padResize,
  printBinary,
  ) where

import Numeric
import Data.Char

padResize :: Int -> a -> [a] -> [a]
padResize targetLength pad input = replicate (targetLength - inputLength) pad ++ drop (inputLength - targetLength) input
  where inputLength = length input

printBinary :: (Integral a, Show a) => Int -> a -> String
printBinary digits num = padResize digits '0' (showIntAtBase 2 intToDigit num "")
