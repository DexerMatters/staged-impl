{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Typing where

import AST (Term (..), Type (..))

type Context = ([(String, Type)], [(String, Type)])

data TypeError
  = UnboundVariable String
  | TypeMismatch Type Type
  | TypeMismatch' Type String
  | UnboxOutOfBounds Int Int
  deriving (Show, Eq)

type TypeResult a = Either TypeError a

infer :: Context -> Term -> TypeResult Type
infer ctx@(uctx, xctx) term = case term of
  -- Code
  Var x ->
    case lookup x (uctx <> xctx) of
      Just t -> Right t
      Nothing -> Left (UnboundVariable x)
  Lam x t body ->
    TArrow t <$> infer (uctx, (x, t) : xctx) body
  App f a -> do
    fT <- infer ctx f
    aT <- infer ctx a
    case fT of
      TArrow aT' rT | aT' == aT -> Right rT
      TArrow aT' _ -> Left (TypeMismatch aT' aT)
      _ -> Left (TypeMismatch' fT "function type")
  Let x t body -> do
    t' <- infer ctx t
    bodyT <- infer (uctx, (x, t') : xctx) body
    Right bodyT
  LetBox x t body ->
    infer ctx t >>= \case
      TBox t' -> infer ((x, t') : uctx, xctx) body
      t' -> Left (TypeMismatch' t' "boxed type")
  Box t -> TBox <$> infer (uctx, []) t
  -- Tuple
  Product t1 t2 ->
    TProduct
      <$> infer ctx t1
      <*> infer ctx t2
  Fst t ->
    infer ctx t >>= \case
      TProduct t1 _ -> Right t1
      t' -> Left (TypeMismatch' t' "product type")
  Snd t ->
    infer ctx t >>= \case
      TProduct _ t2 -> Right t2
      t' -> Left (TypeMismatch' t' "product type")
  -- Primary Type
  Succ t ->
    infer ctx t >>= \case
      TNat -> Right TNat
      t' -> Left (TypeMismatch' t' "nat type")
  Zero -> Right TNat
  Unit -> Right TUnit
  -- Recursive
  Fix x t body ->
    infer (uctx, (x, t) : xctx) body
  -- Branch
  Case s a x a' ->
    infer ctx s >>= \case
      TNat -> do
        aT <- infer ctx a
        a'T <- infer (uctx, (x, TNat) : xctx) a'
        if aT == a'T
          then Right aT
          else Left (TypeMismatch aT a'T)
      t' -> Left (TypeMismatch' t' "nat type")