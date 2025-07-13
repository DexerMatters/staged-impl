{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Eval where

import AST (Term (..), Type, isRuntimeType)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Typing (StageFlag (..))

data Value
  = VLam String Type (Value -> Value)
  | VProduct Value Value
  | VUnit
  | VNat Int
  | VLazy Env Term

data Term'
  = Var' String
  | Lam' String Type Term'
  | Let' String Term' Term'
  | App' Term' Term'
  | Product' Term' Term'
  | Fst' Term'
  | Snd' Term'
  | Unit'
  | Zero'
  | Succ' Term'
  | Case' Term' Term' String Term'
  | Fix' String Type Term'
  | CT' Term'
  deriving (Eq)

type Env = ([(String, Value)], [(String, Value)])

instance Show Value where
  show (VLam x _ _) = "fun (" ++ x ++ ")"
  show (VProduct t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show VUnit = "unit"
  show (VNat n) = show n
  show (VLazy {}) = "{ ... }"

show' :: Value -> String
show' (VLam x _ _) = "fun (" ++ x ++ ")"
show' (VProduct t1 t2) = "(" ++ show' t1 ++ ", " ++ show' t2 ++ ")"
show' VUnit = "unit"
show' (VNat n) = show n
show' (VLazy env term) = "{" ++ refinedShow env term ++ "}"

eval :: StageFlag -> Env -> Term -> Value
eval CompTime env@(uenv, xenv) = \case
  CT (App f a) -> case evalC env f of
    VLam _ _ body -> body (evalC env a)
    _ -> error "Not a function"
  CT (Let x t body) ->
    let v = evalC env t
     in evalC ((x, v) : uenv, xenv) body
  CT (Product t1 t2) ->
    VProduct (evalC env t1) (evalC env t2)
  CT (Fst t) ->
    let (VProduct v1 _) = evalC env t in v1
  CT (Snd t) ->
    let (VProduct _ v2) = evalC env t in v2
  CT Unit -> VUnit
  CT Zero -> VNat 0
  CT (Succ t) -> VNat (1 + case evalC env t of VNat n -> n; _ -> 0)
  CT (Case s a x a') ->
    let (VNat n) = evalC env s
     in if n == 0
          then evalC env a
          else evalC ((x, VNat (n - 1)) : uenv, xenv) a'
  e@(CT (Fix x _ body)) ->
    evalC ((x, VLazy env e) : uenv, xenv) body
  Lam x t body
    | isRuntimeType t ->
        VLazy env (Lam x t body)
    | otherwise ->
        VLam x t $ \v -> evalC ((x, v) : uenv, xenv) body
  Var x ->
    case lookup x uenv of
      Just (VLazy env' v) -> evalC env' v
      Just v -> v
      Nothing -> case lookup x xenv of
        Just _ -> VLazy env (Var x)
        _ -> error "Never reach"
  Zero -> VNat 0
  e@(Fix x t body)
    | not $ isRuntimeType t ->
        evalC ((x, VLazy env e) : uenv, xenv) body
    | otherwise ->
        VLazy env e
  t -> VLazy env t
eval Runtime env@(uenv, xenv) = \case
  Var x ->
    case fromJust $ lookup x (uenv <> xenv) of
      VLazy env' v -> evalR env' v
      v -> v
  Lam x t body ->
    VLam x t $ \v -> evalR (uenv, (x, v) : xenv) body
  Let x t body ->
    let v = evalR env t
     in evalR (uenv, (x, v) : xenv) body
  App f a ->
    let (VLam _ _ body) = evalR env f
        v = evalR env a
     in body v
  Product t1 t2 ->
    VProduct (evalR env t1) (evalR env t2)
  Fst t ->
    let (VProduct v1 _) = evalR env t in v1
  Snd t ->
    let (VProduct _ v2) = evalR env t in v2
  Unit -> VUnit
  Zero -> VNat 0
  Succ t ->
    VNat (1 + case evalR env t of VNat n -> n; _ -> 0)
  Case s a x a' ->
    let (VNat n) = evalR env s
     in if n == 0
          then evalR env a
          else evalR (uenv, (x, VNat (n - 1)) : xenv) a'
  e@(Fix x _ body) ->
    evalR (uenv, (x, VLazy env e) : xenv) body
  CT t -> evalR env t

refinedShow :: Env -> Term -> String
refinedShow env = \case
  Var x ->
    maybe x show' (lookup x (uncurry (<>) env))
  Lam x t body ->
    "fun " ++ x ++ " : " ++ show t ++ ". " ++ refinedShow env body
  Let x t body ->
    "let " ++ x ++ " = " ++ refinedShow env t ++ " in " ++ refinedShow env body
  App f a ->
    "(" ++ refinedShow env f ++ ") (" ++ refinedShow env a ++ ")"
  Product t1 t2 ->
    "(" ++ refinedShow env t1 ++ ", " ++ refinedShow env t2 ++ ")"
  Fst t ->
    "fst(" ++ refinedShow env t ++ ")"
  Snd t ->
    "snd(" ++ refinedShow env t ++ ")"
  Unit -> "unit"
  Zero -> "0"
  Succ t -> "1 + " ++ refinedShow env t
  Fix x t body ->
    "fix " ++ x ++ " : " ++ show t ++ ". " ++ refinedShow env body
  Case s a x a' ->
    "case "
      ++ refinedShow env s
      ++ " of 0 => "
      ++ refinedShow env a
      ++ " | s "
      ++ x
      ++ " => "
      ++ refinedShow env a'
  CT t -> refinedShow env t

evalC :: Env -> Term -> Value
evalC = eval CompTime

evalR :: Env -> Term -> Value
evalR = eval Runtime

tr :: (Show a) => String -> a -> a
tr msg a = trace (msg ++ " :" ++ show a) a

tr' :: String -> Value -> Value
tr' msg a = trace (msg ++ " :" ++ show' a) a

trM :: (Show a, Monad m) => String -> a -> m a
trM msg a = do
  trace (msg ++ " :" ++ show a) $ return a