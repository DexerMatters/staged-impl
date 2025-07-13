{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Eval where

import AST (Env, Term (..), Value (..))
import Data.Maybe (fromJust)

eval :: Env -> Term -> Value
eval env = \case
  Var x -> case fromJust $ lookup x env of
    VLazy env' body -> eval env' body
    v -> v
  Lam x t body -> VLam x t env body
  App f a ->
    let (VLam x _ env' body) = eval env f
        a' = eval env a
     in eval ((x, a') : env') body
  Let x t body ->
    eval ((x, eval env t) : env) body
  Box t -> VBox env t
  Unbox _ t ->
    let (VBox env' body) = eval env t
     in eval env' body
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
          VNat n | n > 0 -> eval ((x, VNat $ n - 1) : env) a'
          _ -> error "Never reach"
  e@(Fix x _ body) ->
    eval ((x, VLazy env e) : env) body
  Value v -> v

quote :: Value -> Term
quote = \case
  VBox env t -> Box (extend env t)
  VLazy env t -> extend env t
  VLam x t env body -> Lam x t (extend env body)
  VProduct v1 v2 -> Product (quote v1) (quote v2)
  VUnit -> Unit
  VNat n -> Value (VNat n)

unbox :: Term -> Term
unbox = \case
  Box t -> t
  v -> v

extend :: Env -> Term -> Term
extend env = \case
  Var x -> case lookup x env of
    Just v -> quote v
    Nothing -> Var x
  Lam x t body -> Lam x t (extend env body)
  App f a -> App (extend env f) (extend env a)
  Let x t body -> Let x (extend env t) (extend env body)
  Box t -> Box (extend env t)
  Unbox n t -> Unbox n (extend env t)
  Product t1 t2 -> Product (extend env t1) (extend env t2)
  Fst t -> Fst (extend env t)
  Snd t -> Snd (extend env t)
  Unit -> Unit
  Zero -> Zero
  Succ t -> Succ (extend env t)
  Case s a x a' -> Case (extend env s) (extend env a) x (extend env a')
  Fix x t body -> Fix x t (extend env body)
  Value v -> Value v
