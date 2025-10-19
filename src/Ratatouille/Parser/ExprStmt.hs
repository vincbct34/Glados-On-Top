{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Expression and statement parsers
-}

module Ratatouille.Parser.ExprStmt
  ( pExpr,
    pExprAssign,
    pExprSend,
    pExprAdditive,
    pExprMultiplicative,
    pLet,
    pAssign,
    pStatement,
    pOpMulDiv,
    pOpAddSub,
  )
where

import qualified Data.List as DL
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (pack)
import Ratatouille.AST
  ( Expr
      ( EAssign,
        EBinOp,
        EBlock,
        ECall,
        ELiteral,
        ESend,
        ESpawn,
        ETuple,
        EVar
      ),
    Literal (LInt),
    Op (..),
    Stmt (..),
  )
import Ratatouille.Parser.Common
  ( Parser,
    pAtom,
    pIdentifier,
    pLiteral,
    symbol,
  )
import Text.Megaparsec
  ( MonadParsec (try),
    between,
    many,
    optional,
    sepEndBy,
    (<|>),
  )

-- Parenthesized expression
pParens :: Parser Expr
pParens = between (symbol (pack "(")) (symbol (pack ")")) pExpr

-- Variable or function call parser
pVarOrCall :: Parser Expr
pVarOrCall = do
  name <- pIdentifier
  maybeArgs <- optional (between (symbol (pack "(")) (symbol (pack ")")) (sepEndBy pExpr (symbol (pack ","))))
  case maybeArgs of
    Nothing -> return (EVar name)
    Just args -> return (ECall name args)

-- Brace content (tuples or blocks)
pBraceContent :: Parser Expr
pBraceContent = between (symbol (pack "{")) (symbol (pack "}")) (try pBlockContent <|> pTupleContent)
  where
    pBlockContent = do
      firstCheck <- (Left <$> try (pStatement <* symbol (pack ";"))) <|> (Right <$> try pLet)
      case firstCheck of
        Left stmtWithSemi -> do
          restStmtsWithSemi <- many (try (pStatement <* symbol (pack ";")))
          maybeLastStmt <- optional (try pLet)
          maybeFinalExpr <- if isJust maybeLastStmt then pure Nothing else optional pExpr
          let allStmts = stmtWithSemi : restStmtsWithSemi ++ maybeToList maybeLastStmt
          let finalExpr = fromMaybe (ELiteral (LInt 0)) maybeFinalExpr
          return $ EBlock allStmts finalExpr
        Right letStmt -> do
          maybeSemi <- optional (symbol (pack ";"))
          case maybeSemi of
            Just _ -> do
              restStmtsWithSemi <- many (try (pStatement <* symbol (pack ";")))
              maybeLastStmt <- optional (try pLet)
              maybeFinalExpr <- if isJust maybeLastStmt then pure Nothing else optional pExpr
              let allStmts = letStmt : restStmtsWithSemi ++ maybeToList maybeLastStmt
              let finalExpr = fromMaybe (ELiteral (LInt 0)) maybeFinalExpr
              return $ EBlock allStmts finalExpr
            Nothing -> do
              return $ EBlock [letStmt] (ELiteral (LInt 0))

    pTupleContent = ETuple <$> sepEndBy pExprAdditive (symbol (pack ","))

-- Multiplicative operators
pOpMulDiv :: Parser Op
pOpMulDiv = (Mul <$ symbol (pack "*")) <|> (Div <$ symbol (pack "/"))

-- Additive operators
pOpAddSub :: Parser Op
pOpAddSub = (Add <$ symbol (pack "+")) <|> (Sub <$ symbol (pack "-"))

-- Generic left-associative infix builder
makeInfixL :: Parser Expr -> Parser Op -> Parser Expr
makeInfixL termParser opParser = do
  term <- termParser
  DL.foldl' (\rec (op, nextTerm) -> EBinOp op rec nextTerm) term
    <$> many ((,) <$> opParser <*> termParser)

-- Multiplicative expressions
pExprMultiplicative :: Parser Expr
pExprMultiplicative = makeInfixL pBaseExpr pOpMulDiv

-- Additive expressions
pExprAdditive :: Parser Expr
pExprAdditive = makeInfixL pExprMultiplicative pOpAddSub

-- Send operator (right-associative)
pExprSend :: Parser Expr
pExprSend = do
  left <- pExprAdditive
  maybeOp <- optional (try (ESend <$ symbol (pack "<-")))
  case maybeOp of
    Nothing -> return left
    Just op -> op left <$> pExprSend

-- Assignment expressions (right-associative)
pExprAssign :: Parser Expr
pExprAssign = try pAssignExpr <|> pExprSend
  where
    pAssignExpr = do
      name <- pIdentifier
      _ <- symbol (pack "=")
      EAssign name <$> pExprAssign

-- Public entry for expressions
pExpr :: Parser Expr
pExpr = pExprAssign

-- Base expression re-uses common parsers
pBaseExpr :: Parser Expr
pBaseExpr =
  (ELiteral <$> pLiteral)
    <|> pAtom
    <|> try pSpawn
    <|> pVarOrCall
    <|> pBraceContent
    <|> pParens

-- Spawn expression
pSpawn :: Parser Expr
pSpawn = do
  _ <- symbol (pack "spawn")
  pName <- pIdentifier
  args <- between (symbol (pack "(")) (symbol (pack ")")) (sepEndBy pExpr (symbol (pack ",")))
  return (ESpawn pName args)

-- Statements
pLet :: Parser Stmt
pLet = do
  _ <- symbol (pack "let")
  varName <- pIdentifier
  _ <- symbol (pack "=")
  SLet varName <$> pExpr

pAssign :: Parser Stmt
pAssign = try $ do
  varName <- pIdentifier
  _ <- symbol (pack "=")
  SAssign varName <$> pExpr

pStatement :: Parser Stmt
pStatement = pLet <|> pAssign <|> (SExpr <$> pExpr)
