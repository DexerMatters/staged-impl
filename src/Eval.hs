{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Eval where

import AST (Env, Term (..), Value (..))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

eval :: Env -> Term -> Value
eval env@(uenv, xenv) = \case
  Var x ->
    case fromJust $ lookup x (uenv <> xenv) of
      VLazy env' body -> eval env' body
      v -> v
  Lam x t body -> VLam x t env body
  App f a ->
    let (VLam x _ (uenv', xenv') body) = eval env f
        a' = eval env a
     in eval (uenv', (x, a') : xenv') body
  Let x t body ->
    eval (uenv, (x, eval env t) : xenv) body
  Box t -> VBox env t
  LetBox x t body ->
    let (VBox (uenv', xenv') t') = eval env t
     in eval ((x, VLazy (uenv', xenv') t') : uenv, xenv) body
  Product t1 t2 ->
    let v1 = eval env t1
        v2 = eval env t2
     in VProduct v1 v2
  Fst t ->
    let (VProduct t1 _) = eval env t
     in t1
  Snd t ->
    let (VProduct _ t2) = eval env t
     in t2
  Unit -> VUnit
  Zero -> VNat 0
  Succ t ->
    let (VNat n) = eval env t
     in VNat (n + 1)
  Case s a x a' ->
    let v = eval env s
     in case v of
          VNat 0 -> eval env a
          VNat n | n > 0 -> eval (uenv, (x, VNat $ n - 1) : xenv) a'
          _ -> error "Never reach"
  e@(Fix x _ body) ->
    eval (uenv, (x, VLazy env e) : xenv) body
  Value v -> v

quote :: Value -> Term
quote = \case
  VBox env' term -> extend (uncurry (<>) env') term
  VLazy env' term -> extend (uncurry (<>) env') term
  VLam x t env' body ->
    Lam x t (extend (uncurry (<>) env') body)
  VProduct t1 t2 -> Product (quote t1) (quote t2)
  VUnit -> Unit
  VNat n -> Value (VNat n)

extend :: [(String, Value)] -> Term -> Term
extend env = \case
  Var x -> maybe (Var x) quote (lookup x env)
  Lam x t body -> Lam x t (extend env body)
  App f a -> App (extend env f) (extend env a)
  Let x t body -> Let x (extend env t) (extend env body)
  Box t -> Box (extend env t)
  LetBox x t body -> LetBox x (extend env t) (extend env body)
  Product t1 t2 -> Product (extend env t1) (extend env t2)
  Fst t -> Fst (extend env t)
  Snd t -> Snd (extend env t)
  Unit -> Unit
  Zero -> Zero
  Succ t -> Succ (extend env t)
  Case s a x a' -> Case (extend env s) (extend env a) x (extend env a')
  Fix x t body -> Fix x t (extend env body)
  Value v -> Value v

stageEval :: Value -> Value
stageEval = \case
  VLam x t env body -> VLam x t env body
  VProduct t1 t2 -> VProduct (stageEval t1) (stageEval t2)
  VUnit -> VUnit
  VNat n -> VNat n
  VBox env term -> eval env term
  VLazy env term -> VLazy env term

isFullyEvaluated :: Value -> Bool
isFullyEvaluated = \case
  VLam {} -> True
  VProduct t1 t2 -> isFullyEvaluated t1 && isFullyEvaluated t2
  VUnit -> True
  VNat _ -> True
  VBox {} -> False
  VLazy env term -> isFullyEvaluated (eval env term)

tr :: (Show a) => String -> a -> a
tr msg a = trace (msg ++ " :" ++ show a) a