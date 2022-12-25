import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )

import Frontend (parse)
import Inference.Basic
import Print ( run, runFile, runVectorTree )

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  -m              Parse stdin, print matrix AST."
    , "  (filepaths)     Parse content of files verbosely."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run
    ["-m"] -> getContents >>= runVectorTree
    ["--basic"] -> do
      src <- getContents
      case parse src of
        Left _ -> print "Parse Error"
        Right tree -> print $ typecheckProgram tree
    fs -> mapM_ runFile fs
