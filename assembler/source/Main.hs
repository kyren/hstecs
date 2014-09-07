import Tecs.Assembly

main :: IO ()
main = do
  contents <- getContents
  case assemble contents of
    Left err -> putStrLn $ "Parse Error " ++ err
    Right ops -> putStr $ printOperations ops
