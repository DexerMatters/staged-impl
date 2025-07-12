{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Eval where

import AST (Term (..))
import Data.Maybe (fromJust)
import Debug.Trace (trace)

data Value
  = VLam String (Value -> Value)
  | VProduct Value Value
  | VUnit
  | VNat Int
  | VBox Env Term
  | VLazy Env Term

type Env = ([(String, Value)], [(String, Value)])

instance Show Value where
  show (VLam x _) = "fun (" ++ x ++ ")"
  show (VProduct t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show VUnit = "unit"
  show (VNat n) = show n
  show (VBox _ term) = "[[" ++ show term ++ "]]"
  show (VLazy _ _) = "..."

eval :: Env -> Term -> Value
eval env@(uenv, xenv) = \case
  Var x ->
    case fromJust $ lookup x (uenv <> xenv) of
      VLazy env' body -> eval env' body
      v -> v
  Lam x _ body ->
    VLam x $ \v -> eval (uenv, (x, v) : xenv) body
  App f a ->
    let (VLam _ closure) = eval env f
     in closure (eval env a)
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

stageEval :: Value -> Value
stageEval = \case
  VLam x closure -> VLam x closure
  VProduct t1 t2 -> VProduct (stageEval t1) (stageEval t2)
  VUnit -> VUnit
  VNat n -> VNat n
  VBox env term -> eval env term
  VLazy env term -> VLazy env term

isFullyEvaluated :: Value -> Bool
isFullyEvaluated = \case
  VLam _ _ -> True
  VProduct t1 t2 -> isFullyEvaluated t1 && isFullyEvaluated t2
  VUnit -> True
  VNat _ -> True
  VBox {} -> False
  VLazy env term -> isFullyEvaluated (eval env term)

tr :: (Show a) => String -> a -> a
tr msg a = trace (msg ++ " :" ++ show a) a