module Inference.Basic where

import Control.Monad.Trans.State
import Control.Monad.Trans (lift)
import qualified Data.Set as Set
import Lang.Abs (Exp(..), Ident(Ident), Program(Prog))

data Type
  = Variable String
  | Function Type Type
  | Terminal String
  deriving Eq

type Constraint = (Type, Type)
-- type Context = [(String, Type)]
data Context = Context
  { ctxContext :: [(String, Type)]
  , ctxNames :: [String]
  }

freshContext :: Context
freshContext = Context
  { ctxContext = []
  , ctxNames = ['V' : show x | x <- [1..]]
  }

-- TODO
infer :: Exp -> StateT Context (Either ()) (Type, Constraint)
infer e = case e of
  Lambda (Ident x) exp -> do
    lift $ Left ()
  Succ exp -> do
    lift $ Left ()
  Pred exp -> do
    lift $ Left ()
  Cond exp exp' exp2 -> do
    lift $ Left ()
  Let (Ident x) exp exp' -> do
    lift $ Left ()
  Appl exp exp' -> do
    lift $ Left ()
  Zero -> do
    lift $ Left ()
  Var (Ident x) -> do
    lift $ Left ()

typecheck :: Exp -> Either () Type
typecheck exp = let (t, cs) = infer' exp () in unify t cs

infer' :: Exp -> () -> (Type, [Constraint])
infer' = undefined

unify :: Type -> [Constraint] -> Either () Type
unify t [] = Right t
-- Constraints take form S=T
unify t0 ((s, t):cs)
  | s == t = unify t0 cs
unify t0 ((Variable x, t):cs)
  | x `notFV` t = unify (substitute x t t0) (fmap (substituteConstraint x t) cs)
unify t0 ((s, Variable x):cs)
  | x `notFV` s = unify (substitute x s t0) (fmap (substituteConstraint x s) cs)
unify t0 ((Function s1 s2, Function t1 t2):cs)
  = unify t0 ((s1, t1) : (s2, t2) : cs)
unify t0 ((s, t):cs) = Left ()

notFV :: String -> Type -> Bool
notFV varName ty = not $ Set.member varName (fv ty)

-- freeVars :: Exp -> Set.Set String
-- freeVars term = case term of
--   Lambda (Ident x) exp -> Set.delete x (freeVars exp)
--   Succ exp -> freeVars exp
--   Pred exp -> freeVars exp
--   Cond cond true false -> freeVars cond <> freeVars true <> freeVars false
--   Let (Ident x) exp body -> Set.delete x (freeVars body) <> freeVars exp
--   Appl fun arg -> freeVars fun <> freeVars arg
--   Zero -> Set.empty
--   Var (Ident x) -> Set.singleton x

fv :: Type -> Set.Set String
fv ty = case ty of
  Variable s -> Set.singleton s
  Function arg res -> fv arg <> fv res
  Terminal s -> Set.empty

substitute :: String -> Type -> Type -> Type
substitute varName newType t = case t of
  Variable s -> if s == varName then newType else Variable s
  Function arg res -> Function (substitute varName newType arg) (substitute varName newType res)
  Terminal s -> Terminal s

substituteConstraint :: String -> Type -> Constraint -> Constraint
substituteConstraint varName newType (t, t2) =
  let sub = substitute varName newType
  in (sub t, sub t2)

