{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AST where

data Type
  = TNat
  | TArrow Type Type
  | TProduct Type Type
  | TUnit
  | TBox Type
  deriving (Eq)

data Term
  = Var String
  | Lam String Type Term
  | Let String Term Term
  | App Term Term
  | Box Term
  | Unbox Int Term
  | Product Term Term
  | Fst Term
  | Snd Term
  | Unit
  | Zero
  | Succ Term
  | Case Term Term String Term
  | Fix String Type Term
  deriving (Eq)

instance Show Type where
  show TNat = "nat"
  show (TArrow t1 t2) = show t1 ++ " -> " ++ show t2
  show (TProduct t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show TUnit = "unit"
  show (TBox t) = "[[" ++ show t ++ "]]"

instance Show Term where
  show (Var x) = x
  show (Lam x t body) = "fun " ++ x ++ " : " ++ show t ++ " . " ++ show body
  show (Let x t body) = "let " ++ x ++ " = " ++ show t ++ " in " ++ show body
  show (App f a) = "(" ++ show f ++ ") (" ++ show a ++ ")"
  show (Box t) = "[[" ++ show t ++ "]]"
  show (Unbox n t) = "eval " ++ show n ++ " (" ++ show t ++ ")"
  show (Product t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (Fst t) = "fst (" ++ show t ++ ")"
  show (Snd t) = "snd (" ++ show t ++ ")"
  show Unit = "unit"
  show Zero = "0"
  show (Succ t) = "s(" ++ show t ++ ")"
  show (Case s a x a') =
    "case "
      ++ show s
      ++ " of 0 => "
      ++ show a
      ++ " | s "
      ++ x
      ++ " => "
      ++ show a'
  show (Fix x t body) = "fix " ++ x ++ " : " ++ show t ++ ". " ++ show body