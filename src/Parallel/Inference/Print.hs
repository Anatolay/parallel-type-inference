module Parallel.Inference.Print where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native (run)

import qualified Prelude as P
import Parallel.AST
import Parallel.Inference.Algo
import Parallel.Inference.Types
import Print (showVectorTree)

-- | Given a tabular AST, perform type inference.
--
-- For debug and presentation purposes only.
-- Prints intermediate results to stdout
runPrintSteps :: VectorTree -> Vector Int -> P.IO ()
runPrintSteps vectorTree initialTypes = do
  let vectorTreeAcc = use vectorTree
  let initialTypesAcc = use initialTypes
  let (_, initialTypeMatrixAcc) = bindVars vectorTreeAcc initialTypesAcc
  let initialTypeMatrix = run initialTypeMatrixAcc

  P.putStrLn "STEP 1: INITIALIZE"
  P.putStrLn $ showVectorTree vectorTree
  P.putStrLn $ showTypeMatrix initialTypeMatrix
  -- No constraint matrix at this step

  let (_, typeMatrixAcc, constraintMatrixAcc) = letBindings vectorTreeAcc (use initialTypeMatrix)
  let typeMatrix = run typeMatrixAcc
  let constraintMatrix = run constraintMatrixAcc

  P.putStrLn "STEP 2: LET BINDINGS"
  P.putStrLn $ showVectorTree vectorTree
  P.putStrLn $ showTypeMatrix typeMatrix
  P.putStrLn $ showConstraintMatrix constraintMatrix

  let (_, typeMatrixAcc, constraintMatrixAcc) = inferConstants vectorTreeAcc (use typeMatrix) (use constraintMatrix)
  let typeMatrix = run typeMatrixAcc
  let constraintMatrix = run constraintMatrixAcc

  P.putStrLn "STEP 3: INFER CONSTANTS"
  P.putStrLn $ showVectorTree vectorTree
  P.putStrLn $ showTypeMatrix typeMatrix
  P.putStrLn $ showConstraintMatrix constraintMatrix

  let (_, typeMatrixAcc, constraintMatrixAcc) = succTyping vectorTreeAcc (use typeMatrix) (use constraintMatrix)
  let typeMatrix = run typeMatrixAcc
  let constraintMatrix = run constraintMatrixAcc

  P.putStrLn "STEP 4: SUCC AND PRED TYPING"
  P.putStrLn $ showVectorTree vectorTree
  P.putStrLn $ showTypeMatrix typeMatrix
  P.putStrLn $ showConstraintMatrix constraintMatrix

  P.return ()


showTypeMatrix :: TypeMatrix -> P.String
showTypeMatrix (c0, c1, c2) = P.unlines (title : textualRows)
  where
    cols = P.fmap toList [c0, c1, c2]
    stringCols = P.fmap (P.fmap showEncodedType) cols
    textualRows = ("Types" P.++ '\t' : "Args" P.++ '\t' : "Res") : combineLines stringCols
    title = "Type Matrix"

showConstraintMatrix :: ConstraintMatrix -> P.String
showConstraintMatrix cm = P.unlines (title : rows)
  where
    pairs = toList cm
    rows = P.fmap showTypePair pairs
    title = "Constraint Matrix"

showEncodedType :: EncodedType -> P.String
showEncodedType (tag, val)
  | tag P.== natTag = "Nat"
  | tag P.== functionTag = "Fun " P.++ P.show val
  | tag P.== variableTag = "Var " P.++ P.show val
  | tag P.== noneTag = "–"
  | otherwise = "Gibberish"

showTypePair :: TypePair -> P.String
showTypePair (l, r) =
  showEncodedType l P.++ " = " P.++ showEncodedType r

combineLines :: [[P.String]] -> [P.String]
combineLines = P.foldl1 (P.zipWith (\x y -> x P.++ ('\t' : y)))
