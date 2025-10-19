{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Parser
-}

module Ratatouille.Parser
  ( Parser,
    pProgram,
  )
where

import Data.Char (isAlpha, isAlphaNum)
import Data.Text (Text, pack)
import Data.Void (Void)
import Ratatouille.AST (Expr (..), Literal (..), Op (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- ... (sc, symbol, lexeme, isIdentifierChar, isIdentifierStartChar - pas de changement) ...
sc :: Parser ()
sc = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar c = isAlpha c || c == '_'

-- | Parser pour un littéral entier.
pIntLiteral :: Parser Literal
pIntLiteral = LInt <$> lexeme (L.signed sc L.decimal)

-- | Parser pour une chaîne de caractères
pStringLiteral :: Parser Literal
pStringLiteral = lexeme $ do
  _ <- char '"'
  s <- manyTill L.charLiteral (char '"')
  return $ LString (pack s)

-- | Parser pour les littéraux (entiers, chaînes).
pLiteral :: Parser Literal
pLiteral = pIntLiteral <|> pStringLiteral

-- | Parser pour un atome (e.g., :myAtom)
pAtom :: Parser Expr
pAtom = EAtom <$> lexeme (char ':' *> takeWhile1P (Just "atom identifier") isIdentifierChar)

-- | Parser pour une variable (un identifiant).
pVar :: Parser Expr
pVar = lexeme $ do
  start <- takeWhile1P (Just "identifier start") isIdentifierStartChar
  rest <- takeWhileP (Just "identifier character") isIdentifierChar
  return $ EVar (start <> rest)

-- | Parser pour un tuple (e.g., { 1, :foo, bar })
pTuple :: Parser Expr
pTuple = ETuple <$> between (symbol (pack "{")) (symbol (pack "}")) (sepEndBy pExpr (symbol (pack ","))) -- pExpr ici pour permettre toutes les expressions

-- | Parser pour les expressions entre parenthèses.
pParens :: Parser Expr
pParens = between (symbol (pack "(")) (symbol (pack ")")) pExpr -- pExpr car l'expression entre parenthèses peut être complexe

-- | pBaseExpr: Les éléments les plus fondamentaux d'une expression (sans opérateurs infixes).
-- Ce sont les feuilles de l'arbre d'expression.
pBaseExpr :: Parser Expr
pBaseExpr =
  (ELiteral <$> pLiteral)
    <|> pAtom
    <|> pVar
    <|> pTuple
    <|> pParens

-- | pOp Mul/Div: Parser pour les opérateurs de multiplication et division.
pOpMulDiv :: Parser Op
pOpMulDiv = (Mul <$ symbol (pack "*")) <|> (Div <$ symbol (pack "/"))

-- | pOp Add/Sub: Parser pour les opérateurs d'addition et soustraction.
pOpAddSub :: Parser Op
pOpAddSub = (Add <$ symbol (pack "+")) <|> (Sub <$ symbol (pack "-"))

-- | Fonction générique pour parser une séquence d'opérateurs infixes associatifs à gauche.
-- `termParser`: le parser pour un terme de plus haute précédence.
-- `opParser`: le parser pour l'opérateur à ce niveau.
makeInfixL :: Parser Expr -> Parser Op -> Parser Expr
makeInfixL termParser opParser = do
  term <- termParser
  foldl' (\acc (op, nextTerm) -> EBinOp op acc nextTerm) term
    <$> many
      ((,) <$> opParser <*> termParser)

-- | pExprMultiplicative: Gère les multiplications et divisions.
-- Il utilise pBaseExpr comme termes.
pExprMultiplicative :: Parser Expr
pExprMultiplicative = makeInfixL pBaseExpr pOpMulDiv

-- | pExprAdditive: Gère les additions et soustractions.
-- Il utilise pExprMultiplicative comme termes (ce qui implémente la précédence).
pExprAdditive :: Parser Expr
pExprAdditive = makeInfixL pExprMultiplicative pOpAddSub

-- | pExpr: Le parser principal pour une expression, déléguant au plus bas niveau de précédence (additif).
pExpr :: Parser Expr
pExpr = pExprAdditive

-- | pProgram: Le parser de plus haut niveau pour un fichier ou un bloc de code complet.
pProgram :: Parser Expr
pProgram = sc *> pExpr <* sc <* eof
