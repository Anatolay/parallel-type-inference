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

type Depth = Int
type NodeType = Int -- application, let statement, etc
type Node = (Depth, NodeType, String)

toNodeList :: Program -> [Node]
toNodeList (Prog stms)
  = concatMap (expToNodeList 0) stms

-- TODO: node types and values
expToNodeList :: Depth -> Exp -> [Node]
expToNodeList depth (Lambda (Ident x) exp) =
  (depth, 0, "lam") :
  (depth + 1, 0, x) :
  expToNodeList (depth + 1) exp

expToNodeList depth (Succ exp) =
  (depth, 0, "succ") :
  expToNodeList (depth + 1) exp

expToNodeList depth (Pred exp) =
  (depth, 0, "pred") :
  expToNodeList (depth + 1) exp

expToNodeList depth (Cond cond trueExp falseExp) =
  (depth, 0, "if") :
  expToNodeList (depth + 1) cond ++
  expToNodeList (depth + 1) trueExp ++
  expToNodeList (depth + 1) falseExp

expToNodeList depth (Let (Ident x) val body) =
  (depth, 0, "let") :
  (depth + 1, 0, x) :
  expToNodeList (depth + 1) val ++
  expToNodeList (depth + 1) body

expToNodeList depth (Appl fun exp) =
  (depth, 0, "@") :
  expToNodeList (depth + 1) fun ++
  expToNodeList (depth + 1) exp

expToNodeList depth Zero = [(depth, 0, "0")]
expToNodeList depth (Var (Ident x)) = [(depth, 0, x)]

type VectorTree = (Vector Int, Vector Depth, Matrix Char, Vector Int, Vector Int)

astToVectorTree :: Program -> VectorTree
astToVectorTree ast = (indAcc, depthAcc, valueAcc, typeIndAcc, typeAcc)
  where
    nodeList = toNodeList ast

    depths = fmap (\(d, _, _) -> d) nodeList
    -- types  = fmap (\(_, t, _) -> t) nodeList
    values = padToEqualLength '\0' $ fmap (\(_, _, v) -> v) nodeList

    maxLength arr = maximum (map length arr)

    -- Row indexes, might be substituted by node coordinates
    indAcc     = A.fromList (Z :. length values) [0..]

    depthAcc   = A.fromList (Z :. length depths) depths
    valueAcc   = A.fromList (Z :. length values :. maxLength values) (concat values)
    -- Index to refer to the type of another row
    typeIndAcc = A.fromList (Z :. length values) [0..]
    -- Index in type matrix
    typeAcc    = A.fromList (Z :. length values) (repeat $ -1)

type VectorTreeAcc = Acc VectorTree

vectorTreeToAcc :: VectorTree -> Acc VectorTree
vectorTreeToAcc (i, d, v, ti, t) =
  let
    i' = A.use i
    d' = A.use d
    v' = A.use v
    ti' = A.use ti
    t' = A.use t
  in A.lift (i', d', v', ti', t')

padRight :: Int -> a -> [a] -> [a]
padRight n e arr 
  | length arr == n = arr
  | otherwise       = arr ++ replicate (n - length arr) e

padToEqualLength :: a -> [[a]] -> [[a]]
padToEqualLength e arr = map (padRight maxLength e) arr
  where
    maxLength = maximum $ map length arr
