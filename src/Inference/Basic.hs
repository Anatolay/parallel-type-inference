module Inference.Basic where

import Control.Monad.Trans.State ( get, put, StateT(runStateT) )
import Control.Monad.Trans (lift)
import qualified Data.Set as Set
import Lang.Abs (Exp(..), Ident(Ident), Program(Prog))
import qualified Data.List as List

data Type
  = Variable String
  | Function Type Type
  | Nat
  deriving Eq

instance Show Type where
  show Nat = "Nat"
  show (Variable v) = v
  show (Function arg body) =
    let
      argString = case arg of
        Nat -> "Nat"
        Variable v -> v
        Function _ _ -> "(" ++ show arg ++ ")"
    in
      argString ++ " —> " ++ show body

type Constraint = (Type, Type)
type Constraints = [Constraint]
type Gamma = [(Ident, Type)]

data Context = Context
  { ctxGamma :: Gamma
  , ctxConstraints :: Constraints
  , ctxNames :: [String]
  }

instance Show Context where
  show ctx =
    "Context { ctxGamma: " ++ show (ctxGamma ctx) ++ "; ctxConstraints: " ++ show (ctxConstraints ctx)

type TypeChecker a = StateT Context (Either ()) a

freshVar :: TypeChecker Type
freshVar = do
  (Context g c names) <- get
  let (fresh:rest) = names -- (!) might fail on a finite list
  put (Context g c rest)
  pure $ Variable fresh

initContext :: Context
initContext = Context
  { ctxGamma = []
  , ctxConstraints = []
  , ctxNames = ['V' : show x | x <- [1..]]
  }

getGamma :: TypeChecker Gamma
getGamma = fmap ctxGamma get

putGamma :: Gamma -> TypeChecker ()
putGamma gamma = do
  ctx <- get
  put $ ctx { ctxGamma = gamma }

appendGamma :: Ident -> Type -> TypeChecker ()
appendGamma name ty = do
  gamma <- getGamma
  putGamma $ (name, ty) : gamma

getConstraints :: TypeChecker Constraints
getConstraints = fmap ctxConstraints get

putConstraints :: Constraints -> TypeChecker ()
putConstraints cs = do
  ctx <- get
  put $ ctx { ctxConstraints = cs }

appendConstraints :: Type -> Type -> TypeChecker ()
appendConstraints ty ty' = do
  cs <- getConstraints
  putConstraints $ (ty, ty') : cs

askGamma :: String -> Gamma -> Maybe Type
askGamma var [] = Nothing
askGamma var ((Ident name, ty) : rest) = if var == name then Just ty else askGamma var rest

infer :: Exp -> TypeChecker Type
infer e = case e of
  Lambda (Ident x) exp -> do
    bodyTy <- infer exp
    gamma <- getGamma
    argType <- case askGamma x gamma of
      Nothing -> freshVar
      Just ty -> pure ty
    lift $ Right $ Function argType bodyTy
  Succ exp -> do
    ty <- infer exp
    case ty of
      Variable s -> do
        appendConstraints (Variable s) Nat -- TODO: should substitute?
        return Nat
      Function ty' ty2 -> lift $ Left ()
      Nat -> return Nat
  Pred exp -> do
    ty <- infer exp
    case ty of
      Variable s -> do
        appendConstraints (Variable s) Nat -- TODO: should substitute?
        return Nat
      Function ty' ty2 -> lift $ Left ()
      Nat -> return Nat
  Cond cond true false -> do
    condTy <- infer cond
    case condTy of
      Variable s -> do
        appendConstraints (Variable s) Nat -- TODO: should substitute?
      Function ty ty' -> lift $ Left ()
      Nat -> return ()
    trueTy <- infer true
    falseTy <- infer false
    appendConstraints trueTy falseTy
    return trueTy
  Let ident@(Ident x) bind body -> do
    bindTy <- infer bind
    appendGamma ident bindTy
    infer body
  Appl fun arg -> do
    funTy <- infer fun
    argTy <- infer arg
    case funTy of
      Variable s -> do
        fresh <- freshVar
        appendConstraints funTy (Function argTy fresh)
        return fresh
      Function ar body -> do
        appendConstraints ar argTy
        return body
      Nat -> lift $ Left ()
  Zero -> do
    lift $ Right Nat
  Var ident@(Ident x) -> do
    gamma <- getGamma
    case askGamma x gamma of
      Nothing -> do
        newVar <- freshVar
        appendGamma ident newVar
        pure newVar
      Just ty -> pure ty

typecheck :: Exp -> Either () Type
typecheck exp = case runStateT (infer exp) initContext of
  Left () -> Left ()
  Right (ty, ctx) -> unify ty (ctxConstraints ctx)

typecheckProgram :: Program -> [Either () Type]
typecheckProgram (Prog lst) = fmap typecheck lst

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
  Nat -> Set.empty

substitute :: String -> Type -> Type -> Type
substitute varName newType t = case t of
  Variable s -> if s == varName then newType else Variable s
  Function arg res -> Function (substitute varName newType arg) (substitute varName newType res)
  Nat -> Nat

substituteConstraint :: String -> Type -> Constraint -> Constraint
substituteConstraint varName newType (t, t2) =
  let sub = substitute varName newType
  in (sub t, sub t2)

