filepath :: FilePath
filepath = "ifs.lang"

wr :: String -> IO ()
wr = appendFile filepath

gen :: Int -> IO ()
gen depth
  | depth <= 0 = wr "0\n"
  | otherwise = do
      wr "if 0\nthen\n"
      gen (depth - 1)
      wr "else\n"
      gen (depth - 1)

generate :: Int -> IO ()
generate depth = do
  writeFile filepath ""
  gen depth
  appendFile filepath ";"

main :: IO ()
main = do
  d <- readLn
  generate d
