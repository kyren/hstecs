module Tecs.Parsing (
  parseInstructions
) where

import Control.Monad
import Data.Maybe
import Data.Char
import qualified Data.Map as Map
import Text.Parsec
import Tecs.Definitions

compMap :: Map.Map String Comp
compMap = Map.fromList [(compName comp, comp) | comp <- [minBound..maxBound]]

destMap :: Map.Map String Dest
destMap = Map.fromList [(destName dest, dest) | dest <- [minBound..maxBound]]

jumpMap :: Map.Map String Jump
jumpMap = Map.fromList [(jumpName jump, jump) | jump <- [minBound..maxBound]]

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

cCompPart :: Parsec String st Comp
cCompPart = do
  compString <- many1 (noneOf "=;\r\n")
  case Map.lookup compString compMap of
    Just comp -> return comp
    Nothing -> fail $ "Unrecognized comp operator '" ++ compString ++ "'"

cDestPart :: Parsec String st Dest
cDestPart = do
  destString <- many1 (noneOf "=;\r\n")
  _ <- char '='
  case Map.lookup destString destMap of
    Just dest -> return dest
    Nothing -> fail $ "Unrecognized dest operator '" ++ destString ++ "'"

cJumpPart :: Parsec String st Jump
cJumpPart = do
  _ <- char ';'
  jumpString <- many1 (noneOf "=;\r\n")
  case Map.lookup jumpString jumpMap of
    Just jump -> return jump
    Nothing -> fail $ "Unrecognized jump operator '" ++ jumpString ++ "'"

cInstruction :: Parsec String st Instruction
cInstruction = do
  dest <- try cDestPart <|> return DestNULL
  comp <- cCompPart
  jump <- try cJumpPart <|> return JumpNULL
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
