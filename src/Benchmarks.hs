module Benchmarks where

import Criterion
import Criterion.Main
import Criterion.Types
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.PTX as GPU
import Frontend (parseOrEmpty)
import Inference.Basic (typecheckProgram)
import Parallel.Inference.Algo (infer)

benchmarkConfig :: Config
benchmarkConfig = defaultConfig { timeLimit = 30.0 }

runBenchmarks :: Int -> IO ()
runBenchmarks num = defaultMainWith (benchmarkConfig { resamples = num }) testSuite
  where
    testSuite = [testFunctionApplication, testLargeSucc, testApplicationNested, testApplicationFlat, testLambdaNested]

testProg :: String -> String -> Benchmark
testProg name src = bgroup name
  [ bench "Basic" $ nf bf prog
  , bench "Parallel" $ nf pf prog
  ]
  where
    prog = parseOrEmpty src
    bf p = typecheckProgram p
    pf p = GPU.run (A.lift $ infer p)

testFunctionApplication :: Benchmark
testFunctionApplication = testProg "Function application" "(λx.x) 0 ;"

testLargeSucc :: Benchmark
testLargeSucc = testProg "Large nested sequence of succ" $
  concat (replicate 1000000 "succ ") ++ "0 ;"

testApplicationNested :: Benchmark
testApplicationNested = testProg "Lots of lambdas and nested applications" src
  where
    src = abstractions ++ applications ++ "0 ;"
    count = 9000

    abstractions = concat $ replicateN count $ \n -> concat ["let f", show n, " = λx", show n, ".x", show n, " in "]
    applications = concat $ replicateN count $ \n -> "f" ++ show n ++ " "

testApplicationFlat :: Benchmark
testApplicationFlat = testProg "Lots of lambdas and sequential applications" src
  where
    src = abstractions ++ applications ++ "0 ;"
    count = 9000

    abstractions = concat $ replicateN count $ \n -> concat ["let f", show n, " = λx", show n, ".x", show n, " in "]
    applications = concat $ replicateN count $ \n -> concat ["let a", show n, " = f", show n, " 0 in "]

testLambdaNested :: Benchmark
testLambdaNested = testProg "Nested lambda abstractions" src
  where
    src = concat ["(", function, ") ", zeros, ";"]
    count = 9000

    lambdas = concat $ replicateN count $ \n -> concat ["λx", show n, "."]
    function = lambdas ++ "x" ++ show count
    zeros = concat $ replicate count "0 "



-- UTILS --

replicateN :: Int -> (Int -> a) -> [a]
replicateN n f
  | n > 0 = f n : replicateN (n-1) f
  | otherwise = []
