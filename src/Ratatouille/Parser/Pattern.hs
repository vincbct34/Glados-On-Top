{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Pattern matching parsers for receive and match expressions
-}
{-# LANGUAGE LambdaCase #-}

module Ratatouille.Parser.Pattern
  ( pPattern,
    pReceiveCase,
    pTypedPattern,
  )
where

import Data.Text (Text, pack)
import Ratatouille.AST (Pattern (..), ReceiveCase (..))
import Ratatouille.Parser.Common
  ( Parser,
    isIdentifierChar,
    lexeme,
    pIdentifier,
    pLiteral,
    pType,
    symbol,
  )
import Ratatouille.Parser.ExprStmt (pExpr)
import Text.Megaparsec
  ( MonadParsec (takeWhile1P, try),
    between,
    optional,
    sepEndBy,
    (<|>),
  )
import Text.Megaparsec.Char (char)

-- | Parse a pattern (basic, no type annotations)
-- Used in simple pattern matching contexts
pPattern :: Parser Pattern
pPattern =
  try pVarargs
    <|> (PLiteral <$> pLiteral)
    <|> (PAtom <$> parseAtom)
    <|> try pTuplePattern
    <|> (PWildcard <$ symbol (pack "_"))
    <|> (PVar <$> pIdentifier)

-- | Parse atom identifier in pattern
parseAtom :: Parser Text
parseAtom = lexeme (char ':' *> takeWhile1P (Just "atom") isIdentifierChar)

-- | Parse a typed pattern (supports type annotations)
-- Used in function signatures and typed destructuring
-- Examples:
--   x           → PVarTyped "x" Nothing False
--   x<i32>      → PVarTyped "x" (Just (TNumeric I32)) False
--   _           → PWildcard
--   {a, b<i32>} → PTuple [PVarTyped "a" Nothing False, PVarTyped "b" (Just (TNumeric I32)) False]
pTypedPattern :: Parser Pattern
pTypedPattern =
  try pVarargs
    <|> (PLiteral <$> pLiteral)
    <|> (PAtom <$> lexeme (char ':' *> takeWhile1P (Just "atom") isIdentifierChar))
    <|> try pTypedTuplePattern
    <|> (PWildcard <$ symbol (pack "_"))
    <|> try pTypedVar
    <|> (PVar <$> pIdentifier)

-- | Parse a typed variable pattern: name<type>
pTypedVar :: Parser Pattern
pTypedVar = do
  name <- pIdentifier
  maybeType <- optional (between (symbol (pack "<")) (symbol (pack ">")) pType)
  return $ PVarTyped name maybeType False

-- | Parse a tuple pattern (basic patterns inside, at least 2 elements)
-- Examples:
--   (a, b)         → PTuple [PVar "a", PVar "b"]
--   (:ok, value)   → PTuple [PAtom "ok", PVar "value"]
pTuplePattern :: Parser Pattern
pTuplePattern = between (symbol (pack "(")) (symbol (pack ")")) $ do
  firstPat <- pPattern
  optional (symbol (pack ",")) >>= \case
    Just _ -> do
      restPats <- sepEndBy pPattern (symbol (pack ","))
      if null restPats
        then fail "Tuple pattern must have at least 2 elements"
        else return $ PTuple (firstPat : restPats)
    Nothing -> return firstPat -- Just a parenthesized pattern

-- | Parse a typed tuple pattern (typed patterns inside, at least 2 elements)
-- Examples:
--   (:add, a<i32>, b<i32>)
--   (x, y<f64>, _)
pTypedTuplePattern :: Parser Pattern
pTypedTuplePattern = between (symbol (pack "(")) (symbol (pack ")")) $ do
  firstPat <- pTypedPattern
  optional (symbol (pack ",")) >>= \case
    Just _ -> do
      restPats <- sepEndBy pTypedPattern (symbol (pack ","))
      if null restPats
        then fail "Tuple pattern must have at least 2 elements"
        else return $ PTuple (firstPat : restPats)
    Nothing -> return firstPat -- Just a parenthesized pattern

-- | Variadic pattern: name...
-- Captures remaining elements in tuples/arrays
pVarargs :: Parser Pattern
pVarargs = do
  name <- pIdentifier
  _ <- symbol (pack "...")
  return $ PVarargs name

-- | Parse a receive case: | pattern -> expression
pReceiveCase :: Parser ReceiveCase
pReceiveCase = do
  _ <- symbol (pack "|")
  pat <- pTypedPattern -- Use typed patterns for receive cases
  _ <- symbol (pack "->")
  Case pat <$> pExpr
