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
preserved = ["let", "case", "of", "fix", "eval", "fun"]

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
mkNumber 0 = Zero
mkNumber n = Succ (mkNumber (n - 1))

parseNumber :: Parser Term
parseNumber = mkNumber . read <$> lexeme (some digitChar)

parseSucc :: Parser Term
parseSucc = Succ <$> (symbol "s" *> between (symbol "(") (symbol ")") parseTerm)

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

parseLet :: Parser Term
parseLet =
  Let
    <$> (symbol "let" *> ident)
    <*> (symbol "=" *> parseTerm)
    <*> (symbol "in" *> parseTerm)

parseBox :: Parser Term
parseBox = Box <$> between (symbol "[[") (symbol "]]") parseTerm

parseUnbox :: Parser Term
parseUnbox =
  Unbox
    <$> (symbol "eval" *> (read <$> lexeme (some digitChar)))
    <*> parseTermFunc

parseProduct :: Parser Term
parseProduct = Product <$> (symbol "(" *> parseTerm <* symbol ",") <*> (parseTerm <* symbol ")")

parseFst :: Parser Term
parseFst = Fst <$> (symbol "fst" *> between (symbol "(") (symbol ")") parseTermFunc)

parseSnd :: Parser Term
parseSnd = Snd <$> (symbol "snd" *> between (symbol "(") (symbol ")") parseTermFunc)

parseUnit :: Parser Term
parseUnit = symbol "unit" $> Unit

parseCase :: Parser Term
parseCase =
  Case
    <$> (symbol "case" *> parseTerm <* symbol "of")
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

parseBoxedType :: Parser Type
parseBoxedType = TBox <$> (symbol "[[" *> parseType <* symbol "]]")

parseTerm :: Parser Term
parseTerm =
  choice
    [ try parseFst,
      try parseSucc,
      try parseSnd,
      try parseApp,
      try parseProduct,
      parseParen,
      parseNumber,
      parseLam,
      parseLet,
      parseBox,
      parseUnbox,
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
      parseBox,
      parseUnit,
      parseVar
    ]

parseType :: Parser Type
parseType =
  choice
    [ try parseArrow,
      parseNat,
      parseUnitType,
      parseProductType,
      parseBoxedType
    ]

parseTypeArrow :: Parser Type
parseTypeArrow =
  choice
    [ parseNat,
      parseUnitType,
      parseProductType,
      parseBoxedType
    ]

parseFromCode :: String -> Either String Term
parseFromCode code =
  case parse (ws >> parseTerm) "" code of
    Left err -> Left (errorBundlePretty err)
    Right term -> Right term