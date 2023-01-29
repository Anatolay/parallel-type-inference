module Inference.Concurrent where
-- import Data.STRef
-- import Control.Monad.ST
-- import Lang.Abs (Exp(..), Ident(Ident), Program(Prog))


-- data Type s
--   = Variable String (STRef s (Maybe (Type s)))
--   | Nat
--   | Function (Type s) (Type s)

-- unify :: Type s -> Type s -> ST s ()
-- unify t1 t2 = do
--   case (t1, t2) of
--     (Nat, Nat) -> return ()
--     (t, Variable v r) -> unify (Variable v r) t
--     (Variable v ref, t) ->
--       if occurs v t
--       then error "can't construct infinite type"
--       else do
--         contents <- readSTRef ref
--         case contents of
--           Nothing -> writeSTRef ref (Just t)
--           Just ty -> unify ty t
--     (Function t1 t2, Function t1' t2') -> do
--       unify t1 t1'
--       unify t2 t2'
--     _ -> error "can't unify"

-- -- TODO
-- infer :: Exp -> ST s (Type s)
-- infer exp = case exp of
--   -- Lambda id exp' -> _
--   -- Succ exp' -> _
--   -- Pred exp' -> _
--   -- Cond exp' exp2 exp3 -> _
--   -- Let id exp' exp2 -> _
--   Appl fun arg -> do
--     funTy <- infer fun
--     argTy <- infer arg
--     (v, ref) <- freshTVar
--     unify funTy (Function argTy (Variable v ref))
--     return $ Variable v ref
--   -- Zero -> _
--   -- Var id -> _
--   _ -> return Nat

-- freshTVar = undefined

-- occurs :: String -> Type s -> Bool
-- occurs = undefined