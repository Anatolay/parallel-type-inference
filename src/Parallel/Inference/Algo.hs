{-# LANGUAGE TypeOperators #-}
module Parallel.Inference.Algo where

import Data.Array.Accelerate as A
import Parallel.Inference.Constraints
import Parallel.Inference.TypeMatrix
import Parallel.Inference.Types
import Parallel.AST

import qualified Prelude as P

run :: VectorTree -> Vector TypeIndex -> VectorTree
run = undefined

bindVars :: Acc VectorTree -> Acc (Vector TypeIndex) -> (Acc VectorTree, Acc TypeMatrix)
bindVars vectorTree typeIndices = (vectorTree, typeMatrix)
  where
    typeMatrix = initTypeMatrixWith typeIndices

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

inferConstants :: Acc VectorTree -> Acc VectorTree
inferConstants = undefined

succTyping :: Acc VectorTree -> Acc VectorTree
succTyping = undefined

lambdaTyping :: Acc VectorTree -> Acc ConstraintMatrix -> (Acc VectorTree, Acc ConstraintMatrix)
lambdaTyping = undefined

applicationTyping :: Acc VectorTree -> Acc ConstraintMatrix -> (Acc VectorTree, Acc ConstraintMatrix)
applicationTyping = undefined

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

