import Tecs.Assembly

main :: IO ()
main = do
  contents <- getContents
  case assemble contents of
    Nothing -> putStrLn "Parse Error"
    Just ops -> putStr $ printOperations ops
