import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )
import Control.Monad.Trans.State (runStateT)
import Lang.Abs ( Program(Prog) )

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as Native

import Frontend (parse)

import Inference.Basic
import Parallel.Inference.Constraints
import Parallel.Inference.TypeMatrix
import Parallel.Inference.Types
import Parallel.Inference.Print
import Parallel.Inference.Algo
import Print ( run, runFile, runVectorTree )
import Parallel.AST (astToVectorTree)


usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  --parse         Parse stdin verbosely."
    , "  -m              Parse stdin, print matrix AST."
    , "  (filepaths)     Parse content of files verbosely."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    ["--parse"] -> getContents >>= run
    ["-m"] -> getContents >>= runVectorTree
    ["--basic"] -> do
      src <- getContents
      case parse src of
        Left _ -> print "Parse Error"
        Right tree -> do
          putStrLn "Parsed"
          print $ typecheckProgram tree
    ["--verbose"] -> do
      src <- getContents
      case parse src of
        Left _ -> print "Parse Error"
        Right prog -> do
          let (vectorTree, initialTypes) = astToVectorTree prog
          runPrintSteps vectorTree initialTypes
    ["--infer", "-v"] -> parallelInfer True
    ["--infer"] -> parallelInfer False
      -- src <- getContents
      -- case parse src of
      --   Left _ -> print "Parse Error"
      --   Right (Prog lst) -> case lst of
      --     [] -> print "Empty"
      --     exp : _ -> print $ runStateT (infer exp) initContext
    fs -> mapM_ runFile fs

parallelInfer :: Bool -> IO ()
parallelInfer isVerbose = do
  src <- getContents
  case parse src of
    Left _ -> print "Parse Error"
    Right prog -> do
      putStrLn "Parsed"
      let (vectorTree, initialTypes) = astToVectorTree prog
      let (typeMatrix, constraintMatrix, b) = Native.run $ A.lift $ performInference vectorTree initialTypes
      print b
      when isVerbose $ putStrLn $ showTypeMatrix typeMatrix

