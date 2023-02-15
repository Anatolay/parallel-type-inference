module Parallel.Inference.Algo where

import Data.Array.Accelerate as A
import Parallel.Inference.Constraints
import Parallel.Inference.TypeMatrix
import Parallel.Inference.Types
import Parallel.AST

import qualified Prelude as P

run :: VectorTree -> VectorTree
run = undefined

bindVars :: Acc VectorTree -> Acc TypeMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
bindVars = undefined

letBindings :: Acc VectorTree -> Acc ConstraintMatrix -> (Acc VectorTree, Acc ConstraintMatrix)
letBindings = undefined

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

