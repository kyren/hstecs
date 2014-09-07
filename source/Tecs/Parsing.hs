module Tecs.Parsing (
  parseInstructions
) where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Tecs.Definitions
import Tecs.Types

lexer :: TokenParser st
lexer = makeTokenParser emptyDef                                   

aConstant :: Parsec String st AValue
aConstant = liftM (AConstant . fromIntegral) $ natural lexer

aName :: Parsec String st AValue
aName = liftM AName $ many1 letter

aInstruction :: Parsec String st Instruction
aInstruction = do
  _ <- char '@'
  liftM AInstruction $ aConstant <|> aName

labelInstruction :: Parsec String st Instruction
labelInstruction = do
  _ <- char '('
  name <- many1 letter
  _ <- char ')'
  return (LabelInstruction name)

cDestPart :: Parsec String st String
cDestPart = do
  dest <- choice $ map string (Map.keys destMap)
  _ <- char '='
  return dest

cCompPart :: Parsec String st String
cCompPart = choice $ map string (Map.keys compMap)

cJumpPart :: Parsec String st String
cJumpPart = do
  _ <- char ';'
  choice $ map string (Map.keys jumpMap)

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
  _ <- many (noneOf "\n")
  return ()

endLinePart :: Parsec String st ()
endLinePart = do
  skipMany (oneOf " \t")
  optional comment
  _ <- char '\n'
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
