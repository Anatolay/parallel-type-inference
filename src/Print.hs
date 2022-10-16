module Print where

import Frontend (getTokens, parseTokens)
import qualified Lang.Print (Print, printTree)
import System.Exit (exitFailure, exitSuccess)

-- | Same as `run`, but accepts a program's source file path instead of a String
runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

-- | Attempt to parse an input string and print resulting AST
run :: String -> IO ()
run s = case parseTokens tokens of
    Left error -> do
      putStrLn "\nParse              Failed...\n"
      putStrLn "Tokens:"
      print tokens
      putStrLn error
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      printTree tree
      exitSuccess
  where
  tokens = getTokens s

-- | Turn AST into (somewhat) prettified String
showTree :: (Lang.Print.Print a) => a -> String
showTree = Lang.Print.printTree

-- | Same as `showTree`, but outputs to stdout
printTree :: (Show a, Lang.Print.Print a) => a -> IO ()
printTree tree
 = do
      putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrLn $ "\n[Linearized tree]\n\n" ++ showTree tree
