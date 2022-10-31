module Parallel where

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
  deriving Eq

nodeTypes :: [NodeType]
nodeTypes = [Variable, LambdaAbstraction, Application, LetExpr, Conditional, SuccOperator, PredOperator, Constant]

indexLookup :: Int -> [a] -> Maybe a
indexLookup _ [] = Nothing
indexLookup n (x:xs)
  | n < 0 = Nothing
  | n == 0 = Just x
  | otherwise = indexLookup (n-1) xs

instance Enum NodeType where
  fromEnum nt = fromMaybe 0 (elemIndex nt nodeTypes)
  toEnum n = fromMaybe Variable (indexLookup n nodeTypes)

type Depth = Int
type Node = (Depth, NodeType, String)

toNodeList :: Program -> [Node]
toNodeList (Prog stms)
  = concatMap (expToNodeList 0) stms

-- TODO: proper values
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

type VectorTree = (Vector Int, Vector Depth, Vector Int, Matrix Char, Vector Int, Vector Int)

astToVectorTree :: Program -> VectorTree
astToVectorTree ast = (indAcc, depthAcc, nodeTypeAcc, valueAcc, typeIndAcc, typeAcc)
  where
    nodeList = toNodeList ast

    depths = fmap (\(d, _, _) -> d) nodeList
    nodeTypes  = fmap (fromEnum . (\(_, t, _) -> t)) nodeList
    values = padToEqualLength '\0' $ fmap (\(_, _, v) -> v) nodeList

    maxLength arr = maximum (map length arr)

    -- Row indexes, might be substituted by node coordinates
    indAcc      = A.fromList (Z :. length values) [0..]

    depthAcc    = A.fromList (Z :. length depths) depths
    nodeTypeAcc = A.fromList (Z :. length depths) nodeTypes
    valueAcc    = A.fromList (Z :. length values :. maxLength values) (concat values)
    -- Index to refer to the type of another row
    typeIndAcc  = A.fromList (Z :. length values) [0..]
    -- Index in type matrix
    typeAcc     = A.fromList (Z :. length values) (repeat $ -1)

type VectorTreeAcc = Acc VectorTree

vectorTreeToAcc :: VectorTree -> Acc VectorTree
vectorTreeToAcc (i, d, nt, v, ti, t) =
  let
    i'  = A.use i
    d'  = A.use d
    nt' = A.use nt
    v'  = A.use v
    ti' = A.use ti
    t'  = A.use t
  in A.lift (i', d', nt', v', ti', t')

padRight :: Int -> a -> [a] -> [a]
padRight n e arr
  | length arr == n = arr
  | otherwise       = arr ++ replicate (n - length arr) e

padToEqualLength :: a -> [[a]] -> [[a]]
padToEqualLength e arr = map (padRight maxLength e) arr
  where
    maxLength = maximum $ map length arr
