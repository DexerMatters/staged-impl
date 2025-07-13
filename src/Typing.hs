{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Typing where

import AST (Term (..), Type (..), compTime, isRuntimeType)
import Debug.Trace (trace)

type Context = ([(String, Type)], [(String, Type)])

data TypeError
  = UnboundVariable String
  | TypeMismatch Type Type
  | TypeMismatch' Type String
  | UnboxOutOfBounds Int Int
  | UnexpectedCompileTimeType Type
  | UnknownTerm Term
  deriving (Show, Eq)

data StageFlag = Runtime | CompTime
  deriving (Show, Eq)

type TypeResult a = Either TypeError a

infer :: StageFlag -> Context -> Term -> TypeResult Type
infer flag ctx@(uctx, xctx) = \case
  -- Function
  Var x ->
    let ctx = if flag == CompTime then uctx else xctx <> uctx
     in case lookup x ctx of
          Just t -> Right t
          Nothing -> Left $ UnboundVariable x
  Lam x t body
    | isRuntimeType t ->
        TArrow t <$> inferR (uctx, (x, t) : xctx) body
    | otherwise ->
        TCompTime . TArrow t
          <$> inferC ((x, t) : uctx, xctx) body
  Let x t body -> do
    t <- inferR ctx t
    inferR (uctx, (x, t) : xctx) body
  CT (Let x t body) -> do
    t <- inferC ctx t
    inferC ((x, t) : uctx, xctx) body
  App f a -> do
    f <- inferR ctx f
    a <- inferR ctx a
    case f of
      TArrow a' r ->
        canConvert a a' >> Right r
      _ -> Left $ TypeMismatch' f "function type"
  CT (App f a) -> do
    f <- inferC ctx f
    a <- inferC ctx a
    case f of
      TCompTime (TArrow a' r) ->
        canConvert a a' >> Right (TCompTime r)
      _ -> Left $ TypeMismatch' f "function type"
  -- Tuple
  Product t1 t2 ->
    TProduct <$> inferR ctx t1 <*> inferR ctx t2
  CT (Product t1 t2) ->
    TCompTime <$> (TProduct <$> inferC ctx t1 <*> inferC ctx t2)
  Fst t ->
    inferR ctx t >>= \case
      TProduct t1 _ -> Right t1
      TCompTime (TProduct t1 _) -> Right t1
      t -> Left $ TypeMismatch' t "product type"
  CT (Fst t) ->
    inferC ctx t >>= \case
      TCompTime (TProduct t1 _) -> Right t1
      t -> Left $ TypeMismatch' t "product type"
  Snd t ->
    inferR ctx t >>= \case
      TProduct _ t2 -> Right t2
      TCompTime (TProduct _ t2) -> Right t2
      t -> Left $ TypeMismatch' t "product type"
  CT (Snd t) ->
    inferC ctx t >>= \case
      TCompTime (TProduct _ t2) -> Right t2
      t -> Left $ TypeMismatch' t "product type"
  Unit -> Right TUnit
  CT Unit -> Right (TCompTime TUnit)
  -- Natural numbers
  Zero -> Right TNat
  CT Zero -> Right (TCompTime TNat)
  Succ t -> do
    t <- inferR ctx t
    canConvert t TNat
    Right TNat
  CT (Succ t) ->
    inferC ctx t >>= \case
      TCompTime TNat -> Right (TCompTime TNat)
      t -> Left $ TypeMismatch' t "nat type"
  -- Case
  Case s a x a' ->
    do
      s <- inferR ctx s
      canConvert s TNat
      a <- inferR ctx a
      a' <- inferR (uctx, (x, TNat) : xctx) a'
      if a == a'
        then Right a
        else Left $ TypeMismatch a a'
  CT (Case s a x a') ->
    inferC ctx s >>= \case
      TCompTime TNat -> do
        a <- inferC ctx a
        a' <- inferC ((x, TCompTime TNat) : uctx, xctx) a'
        canConvert a a'
        Right a'
      t -> Left $ TypeMismatch' t "nat type"
  -- Recursive
  Fix x t body
    | isRuntimeType t ->
        inferR (uctx, (x, t) : xctx) body
    | otherwise ->
        inferC ((x, t) : uctx, xctx) body
  -- Fallback
  u -> Left $ UnknownTerm u
  where
    inferR = infer Runtime
    inferC = infer CompTime

canConvert :: Type -> Type -> Either TypeError ()
canConvert (TCompTime t) (TCompTime t') = canConvert t t'
canConvert (TCompTime t') t = canConvert t' t
canConvert t' t
  | t' == t = Right ()
  | otherwise = Left $ TypeMismatch t' t

tr :: (Show a) => String -> a -> a
tr msg a = trace (msg ++ " :" ++ show a) a

trM :: (Show a, Monad m) => String -> a -> m a
trM msg a = do
  trace (msg ++ " :" ++ show a) $ return a