import Data.Int
import Control.Applicative
import Text.Read
import Numeric
import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Safe
import qualified Data.Map as Map

padResize :: Int -> a -> [a] -> [a]
padResize targetLength pad input = replicate (targetLength - inputLength) pad ++ drop (inputLength - targetLength) input
  where inputLength = length input

printBinary :: (Integral a, Show a) => Int -> a -> String
printBinary digits num = padResize digits '0' (showIntAtBase 2 intToDigit num "")

stripCommentsWhitespace :: String -> Maybe String
stripCommentsWhitespace input = do
  withoutComment <- listToMaybe (splitOn "//" input)
  let withoutSpaces = filter (not . isSpace) withoutComment
  if null withoutSpaces then Nothing else Just withoutSpaces

compMap :: Map.Map String Int8
compMap = Map.fromList [
    ("0", 0x2a),
    ("1", 0x3f),
    ("-1", 0x3a),
    ("D", 0x0c),
    ("A", 0x30),
    ("!D", 0x0d),
    ("!A", 0x31),
    ("-D", 0x0f),
    ("-A", 0x33),
    ("D+1", 0x1f),
    ("A+1", 0x37),
    ("D-1", 0x0e),
    ("A-1", 0x32),
    ("D+A", 0x02),
    ("D-A", 0x13),
    ("A-D", 0x07),
    ("D&A", 0x00),
    ("D|A", 0x15),
    ("M", 0x70),
    ("!M", 0x71),
    ("-M", 0x73),
    ("M+1", 0x77),
    ("M-1", 0x72),
    ("D+M", 0x42),
    ("D-M", 0x43),
    ("M-D", 0x47),
    ("D&M", 0x40),
    ("D|M", 0x55)
  ]

destMap :: Map.Map String Int8
destMap = Map.fromList [
    ("", 0x0),
    ("NULL", 0x0),
    ("M", 0x1),
    ("D", 0x2),
    ("MD", 0x3),
    ("A", 0x4),
    ("AM", 0x5),
    ("AD", 0x6),
    ("AMD", 0x7)
  ]

jumpMap :: Map.Map String Int8
jumpMap = Map.fromList [
    ("", 0x0),
    ("NULL", 0x0),
    ("JGT", 0x1),
    ("JEQ", 0x2),
    ("JGE", 0x3),
    ("JLT", 0x4),
    ("JNE", 0x5),
    ("JLE", 0x6),
    ("JMP", 0x7)
  ]

predefinedVariables :: Map.Map String Int16
predefinedVariables = Map.fromList [
    ("SP", 0),
    ("LCL", 1),
    ("ARG", 2),
    ("THIS", 3),
    ("THAT", 4),
    ("R0", 0),
    ("R1", 1),
    ("R2", 2),
    ("R3", 3),
    ("R4", 4),
    ("R5", 5),
    ("R6", 6),
    ("R7", 7),
    ("R8", 8),
    ("R9", 9),
    ("R10", 10),
    ("R11", 11),
    ("R12", 12),
    ("R13", 13),
    ("R14", 14),
    ("R15", 15),
    ("SCREEN", 16384),
    ("KBD", 24576)
  ]

startingVariableMemory :: Int16
startingVariableMemory = 16

data AValue = AName String | AValue Int16
data Instruction =
  AInstruction AValue |
  LabelInstruction String |
  CInstruction String String String

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

type Symbols = Map.Map String Int16

computeSymbols :: [Instruction] -> Symbols
computeSymbols instructions = addVariables startingVariableMemory (addLabels 0 predefinedVariables instructions) instructions
  where
    addLabels pos labels (LabelInstruction label : rest) = addLabels pos (Map.insert label pos labels) rest
    addLabels pos labels (_ : rest) = addLabels (pos + 1) labels rest
    addLabels _ labels [] = labels
    addVariables nextMemory symbols (AInstruction (AName name) : rest)
      | Map.member name symbols = addVariables nextMemory symbols rest
      | otherwise = addVariables (nextMemory + 1) (Map.insert name nextMemory symbols) rest
    addVariables nextMemory symbols (_ : rest) = addVariables nextMemory symbols rest
    addVariables _ symbols [] = symbols

data Operation = AOperation Int16 | COperation Int8 Int8 Int8

makeOperations :: [Instruction] -> Maybe [Operation]
makeOperations instructions = sequence $ go (computeSymbols instructions) instructions
  where
    go symbols (AInstruction (AName name) : rest) = (AOperation <$> Map.lookup name symbols) : go symbols rest
    go symbols (AInstruction (AValue value) : rest) = Just (AOperation value) : go symbols rest
    go symbols (LabelInstruction _ : rest) = go symbols rest
    go symbols (CInstruction comp dest jump : rest) = (COperation <$> Map.lookup comp compMap <*> Map.lookup dest destMap <*> Map.lookup jump jumpMap) : go symbols rest
    go _ [] = []

assemble :: String -> Maybe [Operation]
assemble content = parseInstructions content >>= makeOperations

printOperations :: [Operation] -> String
printOperations ops = unlines $ map printOperationBinary ops
  where
    printOperationBinary (AOperation num) = '0' : printBinary 15 num 
    printOperationBinary (COperation comp dest jump) = "111" ++ printBinary 7 comp ++ printBinary 3 dest ++ printBinary 3 jump

main :: IO ()
main = do
  contents <- getContents
  case assemble contents of
    Nothing -> putStrLn "Parse Error"
    Just ops -> putStr $ printOperations ops
