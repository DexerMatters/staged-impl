{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AST where

data Type
  = TNat
  | TArrow Type Type
  | TProduct Type Type
  | TUnit
  | TCompTime Type
  deriving (Eq)

data Term
  = Var String
  | Lam String Type Term
  | Let String Term Term
  | App Term Term
  | Product Term Term
  | Fst Term
  | Snd Term
  | Unit
  | Zero
  | Succ Term
  | Case Term Term String Term
  | Fix String Type Term
  | CT Term
  deriving (Eq)

isRuntimeType :: Type -> Bool
isRuntimeType (TCompTime _) = False
isRuntimeType _ = True

compTime :: Type -> Type
compTime (TCompTime t) = TCompTime t
compTime t = TCompTime t

instance Show Type where
  show TNat = "nat"
  show (TCompTime TNat) = "#nat"
  show (TArrow t1 t2) = show t1 ++ " -> " ++ show t2
  show (TCompTime (TArrow t1 t2)) = show t1 ++ " #-> " ++ show t2
  show (TProduct t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (TCompTime (TProduct t1 t2)) = "#(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show TUnit = "unit"
  show (TCompTime TUnit) = "#unit"
  show _ = undefined

instance Show Term where
  show (Var x) = x
  show (Lam x t body) = "fun " ++ x ++ " : " ++ show t ++ " . " ++ show body
  show (Let x t body) = "let " ++ x ++ " = " ++ show t ++ " in " ++ show body
  show (App f a) = "(" ++ show f ++ ") (" ++ show a ++ ")"
  show (Product t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
  show (Fst t) = "fst (" ++ show t ++ ")"
  show (Snd t) = "snd (" ++ show t ++ ")"
  show Unit = "unit"
  show Zero = "0"
  show (CT Zero) = "0"
  show (Succ t) = "s(" ++ show t ++ ")"
  show (CT (Succ t)) = "#s(" ++ show t ++ ")"
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
  show (CT t) = "#(" ++ show t ++ ")"