{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Typing where

import AST (Term (..), Type (..))
import Data.Functor (($>))
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq
import Debug.Trace (trace)

type Context = [(String, Type)]

type CtxStack = Seq Context

data TypeError
  = UnboundVariable String
  | TypeMismatch Type Type
  | TypeMismatch' Type String
  | UnboxOutOfBounds Int Int
  deriving (Show, Eq)

type TypeResult a = Either TypeError a

infer :: CtxStack -> Term -> TypeResult Type
infer (ctx :<| ctxs) term = case term of
  -- Function
  Var x -> case lookup x (concat (ctx :<| ctxs)) of
    Just t -> Right t
    Nothing -> Left (UnboundVariable x)
  Lam x t body ->
    TArrow t <$> infer (((x, t) : ctx) :<| ctxs) body
  App f arg ->
    infer' f >>= \case
      TArrow tArg tRet -> check (ctx :<| ctxs) arg tArg $> tRet
      t -> Left (TypeMismatch' t "function type")
  Let x t body -> do
    tT <- infer (ctx :<| ctxs) t
    infer (((x, tT) : ctx) :<| ctxs) body
  -- Code
  Box t -> TBox <$> infer ([] :<| ctx :<| ctxs) t
  Unbox n t
    | n > length ctxs || n < 0 ->
        Left (UnboxOutOfBounds n (length ctxs))
    -- Reflectivity
    | n == 0 -> infer' t
    -- Unbox
    | otherwise ->
        let ctx' = Seq.drop n $ ctx :<| ctxs
         in infer ctx' t >>= \case
              TBox t' -> Right t'
              t' -> Left (TypeMismatch' t' "box type")
  -- Product
  Product t1 t2 -> TProduct <$> infer' t1 <*> infer' t2
  Fst t ->
    infer' t >>= \case
      TProduct t1 _ -> Right t1
      t' -> Left (TypeMismatch' t' "product type")
  Snd t ->
    infer' t >>= \case
      TProduct _ t2 -> Right t2
      t' -> Left (TypeMismatch' t' "product type")
  -- Natural numbers
  Unit -> Right TUnit
  Zero -> Right TNat
  Succ t ->
    infer' t >>= \case
      TNat -> Right TNat
      t' -> Left (TypeMismatch' t' "natural number type")
  Case s a x a' -> do
    check (ctx :<| ctxs) s TNat
    tA <- infer' a
    tA' <- infer (((x, TNat) : ctx) :<| ctxs) a'
    if tA == tA'
      then Right tA
      else Left (TypeMismatch tA tA')
  -- Recursion
  Fix x t body -> infer (((x, t) : ctx) :<| ctxs) body
  _ -> error "Never reach"
  where
    infer' = infer (ctx :<| ctxs)
infer Seq.Empty term = infer (Seq.singleton []) term

check :: CtxStack -> Term -> Type -> TypeResult ()
check ctx term expectedType = do
  actualType <- infer ctx term
  if actualType == expectedType
    then Right ()
    else Left (TypeMismatch expectedType actualType)

-- Reverse Drop
reverseDrop :: Int -> [a] -> [a]
reverseDrop n xs
  | n <= 0 = xs
  | otherwise = reverse (drop n (reverse xs))

d :: (Show a) => String -> a -> a
d tag a = trace (tag ++ ": " ++ show a) a
