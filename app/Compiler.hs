import System.Environment ( getArgs, getProgName )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )
import Control.Monad.Trans.State (runStateT)
import Lang.Abs ( Program(Prog) )

import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as Native
import qualified Data.Array.Accelerate.LLVM.PTX as GPU
import Prelude as P

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

perform :: (Program -> IO ()) -> IO ()
perform f = do
  src <- getContents
  case parse src of
    Left _ -> putStrLn "Parse Error"
    Right prog -> f prog

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- TODO use normal arg package
    ["--help"] -> usage
    ["--parse"] -> getContents >>= run
    ["-m"] -> getContents >>= runVectorTree
    ["--basic"] -> perform $ \tree -> do
      putStrLn "Parsed"
      print $ typecheckProgram tree
    ["--verbose"] -> perform $ \prog -> do
      let (vectorTree, initialTypes) = astToVectorTree prog
      runPrintSteps vectorTree initialTypes
    ["--infer", "-v"] -> parallelInfer True Cpu
    ["--infer"] -> parallelInfer False Cpu
    ["--infer", "--gpu"] -> parallelInfer False Gpu
    ["--infer", "--gpu", "-v"] -> parallelInfer True Gpu
    ["--infer", "-g"] -> parallelInfer False Gpu
    ["--infer", "-g", "-v"] -> parallelInfer True Gpu
      -- src <- getContents
      -- case parse src of
      --   Left _ -> print "Parse Error"
      --   Right (Prog lst) -> case lst of
      --     [] -> print "Empty"
      --     exp : _ -> print $ runStateT (infer exp) initContext
    fs -> mapM_ runFile fs

type Verbosity = Bool
data Runtime
  = Gpu
  | Cpu

parallelInfer :: Verbosity -> Runtime -> IO ()
parallelInfer isVerbose runtime = perform inference
  where
    run = case runtime of
      Gpu -> GPU.run
      Cpu -> Native.run
    inference prog = do
      putStrLn "Parsed"
      let (vectorTree, initialTypes) = astToVectorTree prog
      let (typeMatrix, constraintMatrix, b) = run $ A.lift $ performInference vectorTree initialTypes
      print b
      when isVerbose $ putStrLn $ showTypeMatrix typeMatrix

