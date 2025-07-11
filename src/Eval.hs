{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Eval where

import AST (Term (..))
import Data.Maybe (fromJust)

data Value
  = VLam String (Value -> Value)
  | VProduct Value Value
  | VUnit
  | VNat Int
  | VBox Env Term
  | VLazy Env Term

type Env = [(String, Value)]

instance Show Value where
  show (VLam x _) = "fun (" ++ x ++ ")"
  show (VProduct t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show VUnit = "unit"
  show (VNat n) = show n
  show (VBox _ term) = "[[" ++ show term ++ "]]"
  show (VLazy _ _) = "..."

eval :: Env -> Term -> Value
eval env = \case
  Var x -> case fromJust $ lookup x env of
    VLazy env' body -> eval env' body
    v -> v
  Lam x _ body -> VLam x $ \v -> eval ((x, v) : env) body
  App f a ->
    let (VLam _ closure) = eval env f
     in closure (eval env a)
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

relabel :: Int -> Int -> Term -> Term
relabel n m = \case
  Var x -> Var x
  Lam x t body -> Lam x t (relabel n m body)
  App f arg -> App (relabel n m f) (relabel n m arg)
  Let x t body -> Let x (relabel n m t) (relabel n m body)
  Box t -> Box (relabel n (m + 1) t)
  Unbox p t
    | p < m -> Unbox p $ relabel n (m - p) t
    | otherwise -> Unbox (p + n - 1) t
  Product t1 t2 -> Product (relabel n m t1) (relabel n m t2)
  Fst t -> Fst (relabel n m t)
  Snd t -> Snd (relabel n m t)
  Unit -> Unit
  Zero -> Zero
  Succ t -> Succ (relabel n m t)
  Case s a x a' -> Case (relabel n m s) (relabel n m a) x (relabel n m a')
  Fix x t body -> Fix x t (relabel n m body)