module Tecs.Parsing (
  parseInstructions
) where

import Control.Monad
import Data.Maybe
import Data.Char
import Text.Parsec
import Tecs.Definitions
import Tecs.Types

positiveNatural :: Parsec String st Integer
positiveNatural = do
  digits <- many1 digit
  return $ foldl (\a i -> a * 10 + fromIntegral (digitToInt i)) 0 digits

asmLabel :: Parsec String st String
asmLabel = do
  s <- letter
  rest <- many $ satisfy $ \x -> (x `notElem` "()") && isPrint x && not (isSpace x)
  return $ s : rest

aConstant :: Parsec String st AValue
aConstant = do
  num <- positiveNatural
  when (num > fromIntegral maximumAConstant) $ fail "Out of bound A-Constant"
  return $ AConstant $ fromIntegral num

aName :: Parsec String st AValue
aName = liftM AName asmLabel

aInstruction :: Parsec String st Instruction
aInstruction = do
  _ <- char '@'
  liftM AInstruction $ aConstant <|> aName

labelInstruction :: Parsec String st Instruction
labelInstruction = do
  _ <- char '('
  name <- asmLabel
  _ <- char ')'
  return (LabelInstruction name)

cCompPart :: Parsec String st String
cCompPart = choice $ map (try . string) sortedComps

cDestPart :: Parsec String st String
cDestPart = do
  dest <- choice $ map (try . string) sortedDests
  _ <- char '='
  return dest

cJumpPart :: Parsec String st String
cJumpPart = do
  _ <- char ';'
  choice $ map (try . string) sortedJumps

cInstruction :: Parsec String st Instruction
cInstruction = do
  dest <- try cDestPart <|> return "NULL"
  comp <- cCompPart
  jump <- try cJumpPart <|> return "NULL"
  return $ CInstruction comp dest jump

instructionPart :: Parsec String st Instruction
instructionPart = aInstruction <|> labelInstruction <|> cInstruction

comment :: Parsec String st ()
comment = do
  _ <- string "//"
  _ <- many (noneOf "\r\n")
  return ()

endLinePart :: Parsec String st ()
endLinePart = do
  skipMany (oneOf " \t")
  optional comment
  _ <- string "\r\n" <|> string "\n"
  return ()

instructionLine :: Parsec String st (Maybe Instruction)
instructionLine = do
  skipMany (oneOf " \t")
  instruction <- instructionPart
  endLinePart
  return $ Just instruction

blankLine :: Parsec String st (Maybe Instruction)
blankLine = do
  endLinePart
  return Nothing

instructionOrBlankLine :: Parsec String st (Maybe Instruction)
instructionOrBlankLine = try blankLine <|> instructionLine

instructions :: Parsec String st [Instruction]
instructions = do
  res <- liftM catMaybes $ many instructionOrBlankLine
  eof
  return res

parseInstructions :: String -> Either String [Instruction]
parseInstructions input = case parse instructions "" input of
  Left err -> Left (show err)
  Right result -> Right result
