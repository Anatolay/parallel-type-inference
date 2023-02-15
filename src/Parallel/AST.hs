module Parallel.AST where

import Data.Array.Accelerate
  ( Matrix
  , Vector
  , Acc
  , Z(Z)
  , (:.) ((:.))
  )
import qualified Data.Array.Accelerate as A
import Lang.Abs (Exp(..), Ident(Ident), Program(Prog))

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

data NodeType
  = Variable
  | LambdaAbstraction
  | Application
  | LetExpr
  | Conditional
  | SuccOperator
  | PredOperator
  | Constant
  deriving (Eq, Enum)

type Depth = Int
type Node = (Depth, NodeType, String)

toNodeList :: Program -> [Node]
toNodeList (Prog stms)
  = concatMap (expToNodeList 0) stms


expToNodeList :: Depth -> Exp -> [Node]
expToNodeList depth (Lambda (Ident x) exp) =
  (depth, LambdaAbstraction, "lam") :
  (depth + 1, Variable, x) :
  expToNodeList (depth + 1) exp

expToNodeList depth (Succ exp) =
  (depth, SuccOperator, "succ") :
  expToNodeList (depth + 1) exp

expToNodeList depth (Pred exp) =
  (depth, PredOperator, "pred") :
  expToNodeList (depth + 1) exp

expToNodeList depth (Cond cond trueExp falseExp) =
  (depth, Conditional, "if") :
  expToNodeList (depth + 1) cond ++
  expToNodeList (depth + 1) trueExp ++
  expToNodeList (depth + 1) falseExp

expToNodeList depth (Let (Ident x) val body) =
  (depth, LetExpr, "let") :
  (depth + 1, Variable, x) :
  expToNodeList (depth + 1) val ++
  expToNodeList (depth + 1) body

expToNodeList depth (Appl fun exp) =
  (depth, Application, "@") :
  expToNodeList (depth + 1) fun ++
  expToNodeList (depth + 1) exp

expToNodeList depth Zero = [(depth, Constant, "0")]
expToNodeList depth (Var (Ident x)) = [(depth, Variable, x)]

type Index = Int
type NodeTag = Int
type VectorTree = (Vector Index, Vector Depth, Vector NodeTag, Matrix Char)

astToVectorTree :: Program -> VectorTree
astToVectorTree ast = (indVec, depthVec, nodeTypeVec, valueMatrix)
  where
    nodeList = toNodeList ast

    depths = fmap (\(d, _, _) -> d) nodeList
    nodeTypes  = fmap (fromEnum . (\(_, t, _) -> t)) nodeList
    values = padToEqualLength '\0' $ fmap (\(_, _, v) -> v) nodeList

    maxLength arr = maximum (map length arr)

    -- Row indexes, might be substituted by node coordinates
    indVec      = A.fromList (Z :. length values) [0..]

    depthVec    = A.fromList (Z :. length depths) depths
    nodeTypeVec = A.fromList (Z :. length depths) nodeTypes
    valueMatrix    = A.fromList (Z :. length values :. maxLength values) (concat values)

type VectorTreeAcc = Acc VectorTree

vectorTreeToAcc :: VectorTree -> Acc VectorTree
vectorTreeToAcc = A.use

padRight :: Int -> a -> [a] -> [a]
padRight n e arr
  | length arr == n = arr
  | otherwise       = arr ++ replicate (n - length arr) e

padToEqualLength :: a -> [[a]] -> [[a]]
padToEqualLength e arr = map (padRight maxLength e) arr
  where
    maxLength = maximum $ map length arr
