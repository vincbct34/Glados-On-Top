{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Pattern parsers
-}

module Ratatouille.Parser.Pattern
  ( pPattern,
    pReceiveCase,
  )
where

import Data.Text (pack)
import Ratatouille.AST (Pattern (..), ReceiveCase (..))
import Ratatouille.Parser.Common
  ( Parser,
    isIdentifierChar,
    lexeme,
    pIdentifier,
    pLiteral,
    symbol,
  )
import Ratatouille.Parser.ExprStmt (pExpr)
import Text.Megaparsec
  ( MonadParsec (takeWhile1P, try),
    between,
    sepEndBy,
    (<|>),
  )
import Text.Megaparsec.Char (char)

pPattern :: Parser Pattern
pPattern =
  try pVarargs
    <|> (PLiteral <$> pLiteral)
    <|> (PAtom <$> lexeme (char ':' *> takeWhile1P (Just "atom") isIdentifierChar))
    <|> (PTuple <$> between (symbol (pack "{")) (symbol (pack "}")) (sepEndBy pPattern (symbol (pack ","))))
    <|> (PWildcard <$ symbol (pack "_"))
    <|> (PVar <$> pIdentifier)

-- Variadic pattern: name...
pVarargs :: Parser Pattern
pVarargs = do
  name <- pIdentifier
  _ <- symbol (pack "...")
  return $ PVarargs name

pReceiveCase :: Parser ReceiveCase
pReceiveCase = do
  _ <- symbol (pack "|")
  pat <- pPattern
  _ <- symbol (pack "->")
  Case pat <$> pExpr
