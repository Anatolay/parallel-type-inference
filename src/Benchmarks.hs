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
    testSuite = [testFunctionApplication, testLargeSucc]

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

