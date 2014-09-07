import System.Environment
import Tecs.Assembly

main :: IO ()
main = do
  [input, output] <- getArgs
  contents <- readFile input
  case assemble contents of
    Left err -> putStrLn $ "Parse Error " ++ err
    Right ops -> writeFile output $ printOperations ops
