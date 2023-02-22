{-# LANGUAGE TypeOperators #-}
module Parallel.Inference.Algo where

import Data.Array.Accelerate as A
import Parallel.Inference.Constraints
import Parallel.Inference.TypeMatrix
import Parallel.Inference.Types
import Parallel.AST

import qualified Prelude as P

runInference :: VectorTree -> Vector TypeIndex -> VectorTree
runInference = undefined

-- | Initialize type matrix for given tabular AST
bindVars :: Acc VectorTree -> Acc (Vector TypeIndex) -> (Acc VectorTree, Acc TypeMatrix)
bindVars vectorTree typeIndices = (vectorTree, typeMatrix)
  where
    typeMatrix = initTypeMatrixWith typeIndices

-- | Infer types of let bindings, collect constraints
letBindings :: Acc VectorTree -> Acc TypeMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
letBindings vectorTree typeMatrix = (vectorTree, typeMatrix, constraints)
  where
    T3 types _ _ = typeMatrix
    letNodeIndices = indicesByNodeType LetExpr vectorTree
    childIndices = childrenIndicesByParent LetExpr vectorTree

    -- For each Let node, there are 3 child nodes
    -- childShape = shape childIndices
    childGroups :: Acc (Matrix Int)
    childGroups = reshape (lift $ Z :. length letNodeIndices :. (3 :: Int)) childIndices

    vars = slice childGroups (constant (Z :. All :. (0 :: Int)))
    bound = slice childGroups (constant (Z :. All :. (1 :: Int)))
    body = slice childGroups (constant (Z :. All :. (2 :: Int)))

    letNodeTypes = map (types !!) letNodeIndices
    varTypes = map (types !!) vars
    boundTypes = map (types !!) bound
    bodyTypes = map (types !!) body

    -- Let expression has the same type as its body
    -- Introduced variable has the same type as the expression
    constraints :: Acc ConstraintMatrix
    constraints = zip letNodeTypes bodyTypes ++ zip varTypes boundTypes

-- | Infer types of known constants
inferConstants :: Acc VectorTree -> Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
inferConstants vectorTree typeMatrix constraintMatrix = (vectorTree, newTypeMatrix, newConstraintMatrix)
  where
    natNodes = indicesByNodeType Constant vectorTree
    T3 types _ _ = typeMatrix

    -- Here we use the fact that at this step in the algorithm,
    -- type matrix only contains type variables, which allows us
    -- to update all of them directly. In a more general case,
    -- this step should only generate more constraints
    vars = map (types !!) natNodes
    (_, indices) = unzip vars
    newTypeMatrix = updateTypeMatrix typeMatrix indices $ fill (shape types) (lift (nat_, 0 :: Int))
    newConstraintMatrix = updateTypeVariables constraintMatrix newTypeMatrix

-- | Infer types of succ and pred expressions
succTyping :: Acc VectorTree -> Acc VectorTree
succTyping = undefined

lambdaTyping :: Acc VectorTree -> Acc ConstraintMatrix -> (Acc VectorTree, Acc ConstraintMatrix)
lambdaTyping = undefined

applicationTyping :: Acc VectorTree -> Acc ConstraintMatrix -> (Acc VectorTree, Acc ConstraintMatrix)
applicationTyping = undefined

-- | When all constraints are collected, resolve them
resolveConstraints :: Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc TypeMatrix, Acc ConstraintMatrix)
resolveConstraints typeMatrix constraintMatrix =
  let
    T3 types args res = typeMatrix

    noRepeats = removeRepeats constraintMatrix
    expanded = expandFunctionEquality noRepeats (zip args res)
    leftified = leftifyTypeVariables expanded
    (indicies, values) = generateTypeMap leftified

    updatedTypeMatrix = updateTypeMatrix typeMatrix indicies values
    T3 newTypes _ _ = updatedTypeMatrix

    substituted = substituteTypeVariables newTypes leftified

    -- TODO: check errors and iterate
  in (updatedTypeMatrix, substituted)

