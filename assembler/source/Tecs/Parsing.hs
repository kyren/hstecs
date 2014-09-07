module Tecs.Parsing (
  parseInstruction,
  parseInstructions
  ) where

import Data.List
import Data.Int
import Data.Maybe
import Data.Char
import Text.Read
import Control.Applicative
import Safe
import Tecs.Types
import Data.List.Split

stripCommentsWhitespace :: String -> Maybe String
stripCommentsWhitespace input = do
  withoutComment <- listToMaybe (splitOn "//" input)
  let withoutSpaces = filter (not . isSpace) withoutComment
  if null withoutSpaces then Nothing else Just withoutSpaces

parseAInstruction :: String -> Maybe Instruction
parseAInstruction input
    | null input = Nothing
    | head input /= '@' = Nothing
    | isJust numValue = Just $ AInstruction $ AValue $ fromJust numValue
    | otherwise = Just $ AInstruction $ AName (tail input)
  where
    numValue = readMaybe (tail input) :: Maybe Int16

parseLabelInstruction :: String -> Maybe Instruction
parseLabelInstruction input = do
  '(' <- headMay input
  label <- initMay (drop 1 input)
  ')' <- lastMay input
  return $ LabelInstruction label

parseCInstruction :: String -> Maybe Instruction
parseCInstruction input = Just $ CInstruction comp dest jump
  where
    eqIndex = fromMaybe (-1) (elemIndex '=' input)
    semIndex = fromMaybe (length input) (elemIndex ';' input)
    comp = take semIndex (drop (eqIndex + 1) input)
    dest = take eqIndex input
    jump = drop (semIndex + 1) input

parseInstruction :: String -> Maybe Instruction
parseInstruction i = parseAInstruction i <|> parseLabelInstruction i <|> parseCInstruction i

parseInstructions :: String -> Maybe [Instruction]
parseInstructions ls = mapM parseInstruction $ mapMaybe stripCommentsWhitespace (lines ls)
