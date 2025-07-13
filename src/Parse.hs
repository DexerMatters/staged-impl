{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Parse where

import AST (Term (..), Type (..))
import Control.Applicative (many, (<|>))
import Control.Monad (guard)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, between, choice, parse, some)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

preserved :: [String]
preserved = ["let", "case", "of", "fix", "fun"]

ws :: Parser ()
ws = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser String -> Parser String
lexeme = L.lexeme ws

symbol :: String -> Parser String
symbol = L.symbol ws

ident :: Parser String
ident = lexeme $ do
  hd <- letterChar
  tl <- many (alphaNumChar <|> char '_')
  guard ((hd : tl) `notElem` preserved)
  return (hd : tl)

mkNumber :: Int -> Term
mkNumber 0 = CT Zero
mkNumber n = CT (Succ (mkNumber (n - 1)))

parseNumber :: Parser Term
parseNumber = mkNumber . read <$> lexeme (some digitChar)

parseSucc :: Parser Term
parseSucc = Succ <$> (symbol "s" *> between (symbol "(") (symbol ")") parseTerm)

parseCTSucc :: Parser Term
parseCTSucc = fmap CT $ Succ <$> (symbol "#s" *> between (symbol "(") (symbol ")") parseTerm)

parseVar :: Parser Term
parseVar = Var <$> ident

parseLam :: Parser Term
parseLam =
  Lam
    <$> (symbol "fun" *> ident)
    <*> (symbol ":" *> parseType)
    <*> (symbol "." *> parseTerm)

parseApp :: Parser Term
parseApp = App <$> parseTermFunc <*> between (symbol "(") (symbol ")") parseTerm

parseCTApp :: Parser Term
parseCTApp = fmap CT $ App <$> parseTermFunc <*> between (symbol "#(") (symbol ")") parseTerm

parseLet :: Parser Term
parseLet =
  Let
    <$> (symbol "let" *> ident)
    <*> (symbol "=" *> parseTerm)
    <*> (symbol "in" *> parseTerm)

parseCTLet :: Parser Term
parseCTLet = fmap CT $ Let <$> (symbol "#let" *> ident) <*> (symbol "=" *> parseTerm) <*> (symbol "in" *> parseTerm)

parseProduct :: Parser Term
parseProduct = Product <$> (symbol "(" *> parseTerm <* symbol ",") <*> (parseTerm <* symbol ")")

parseCTProduct :: Parser Term
parseCTProduct = fmap CT $ Product <$> (symbol "#(" *> parseTerm) <*> (symbol "," *> parseTerm <* symbol ")")

parseFst :: Parser Term
parseFst = Fst <$> (symbol "fst" *> between (symbol "(") (symbol ")") parseTermFunc)

parseCTFst :: Parser Term
parseCTFst = fmap CT $ Fst <$> (symbol "#fst" *> between (symbol "(") (symbol ")") parseTermFunc)

parseSnd :: Parser Term
parseSnd = Snd <$> (symbol "snd" *> between (symbol "(") (symbol ")") parseTermFunc)

parseCTSnd :: Parser Term
parseCTSnd = fmap CT $ Snd <$> (symbol "#snd" *> between (symbol "(") (symbol ")") parseTermFunc)

parseUnit :: Parser Term
parseUnit = symbol "unit" $> Unit

parseCTUnit :: Parser Term
parseCTUnit = fmap CT $ symbol "#unit" $> Unit

parseCase :: Parser Term
parseCase =
  Case
    <$> (symbol "case" *> parseTerm <* symbol "of")
    <*> (symbol "0" *> symbol "=>" *> parseTerm)
    <*> (symbol "|" *> symbol "s" *> ident)
    <*> (symbol "=>" *> parseTerm)

parseCTCase :: Parser Term
parseCTCase =
  fmap CT $
    Case
      <$> (symbol "#case" *> parseTerm <* symbol "of")
      <*> (symbol "0" *> symbol "=>" *> parseTerm)
      <*> (symbol "|" *> symbol "s" *> ident)
      <*> (symbol "=>" *> parseTerm)

parseFix :: Parser Term
parseFix =
  Fix
    <$> (symbol "fix" *> ident)
    <*> (symbol ":" *> parseType)
    <*> (symbol "." *> parseTerm)

parseParen :: Parser Term
parseParen = between (char '(') (char ')') parseTerm

parseNat :: Parser Type
parseNat = TNat <$ symbol "Nat"

parseUnitType :: Parser Type
parseUnitType = TUnit <$ symbol "Unit"

parseArrow :: Parser Type
parseArrow = TArrow <$> (parseTypeArrow <* symbol "->") <*> parseTypeArrow

parseProductType :: Parser Type
parseProductType = TProduct <$> (symbol "(" *> parseType <* symbol ",") <*> (parseType <* symbol ")")

parseCompTimeType :: Parser Type
parseCompTimeType = TCompTime <$> (symbol "#" *> parseTypeSharp)

parseParenType :: Parser Type
parseParenType = between (char '(') (char ')') parseType

parseTerm :: Parser Term
parseTerm =
  choice
    [ try parseCTFst,
      try parseFst,
      try parseCTSucc,
      try parseSucc,
      try parseCTSnd,
      try parseSnd,
      try parseCTApp,
      try parseApp,
      try parseCTProduct,
      try parseProduct,
      try parseCTLet,
      try parseCTUnit,
      try parseCTCase,
      parseParen,
      parseNumber,
      parseLam,
      parseLet,
      parseUnit,
      parseCase,
      parseFix,
      parseVar
    ]

parseTermFunc :: Parser Term
parseTermFunc =
  choice
    [ -- try parseApp,
      try parseProduct,
      parseParen,
      parseNumber,
      parseUnit,
      parseVar
    ]

parseType :: Parser Type
parseType =
  choice
    [ try parseArrow,
      parseNat,
      parseUnitType,
      parseCompTimeType,
      try parseProductType,
      parseParenType
    ]

parseTypeArrow :: Parser Type
parseTypeArrow =
  choice
    [ parseNat,
      parseUnitType,
      parseCompTimeType,
      try parseProductType,
      parseParenType
    ]

parseTypeSharp :: Parser Type
parseTypeSharp =
  choice
    [ parseNat,
      parseUnitType,
      parseCompTimeType,
      try parseProductType,
      parseParenType
    ]

parseFromCode :: String -> Either String Term
parseFromCode code =
  case parse (ws *> parseTerm) "" code of
    Left err -> Left (errorBundlePretty err)
    Right term -> Right term