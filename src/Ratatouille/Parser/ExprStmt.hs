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
    pReceive,
  )
where

import qualified Data.List as DL
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Text (pack)
import Ratatouille.AST
  ( Expr
      ( EAssign,
        EAtom,
        EBinOp,
        EBlock,
        ECall,
        EFieldAccess,
        EIf,
        ELiteral,
        EReceive,
        ESelf,
        ESend,
        ESpawn,
        ETuple,
        EVar
      ),
    Literal (LInt),
    Op (..),
    Pattern (..),
    ReceiveCase (..),
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
  ( MonadParsec (notFollowedBy, try),
    between,
    many,
    optional,
    sepEndBy,
    (<|>),
  )
import Text.Megaparsec.Char (char)

-- Parenthesized expression
pParens :: Parser Expr
pParens = between (symbol (pack "(")) (symbol (pack ")")) pExpr

-- Variable or function call parser with field access support
pVarOrCall :: Parser Expr
pVarOrCall = do
  name <- pIdentifier
  maybeArgs <- optional (between (symbol (pack "(")) (symbol (pack ")")) (sepEndBy pExpr (symbol (pack ","))))
  let baseExpr = case maybeArgs of
        Nothing -> EVar name
        Just args -> ECall name args
  -- Handle field access chaining (e.g., state.field1.field2)
  fields <- many (symbol (pack ".") *> pIdentifier)
  return $ DL.foldl' EFieldAccess baseExpr fields

-- Brace content (tuples, blocks, or single expressions)
pBraceContent :: Parser Expr
pBraceContent = between (symbol (pack "{")) (symbol (pack "}")) (try pBlockContent <|> pExprOrTuple)
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

    -- Parse expression or tuple
    -- In Ratatouille, {} braces ALWAYS create tuples (not expression grouping)
    -- {} → empty tuple
    -- { 123 } → single-element tuple
    -- { 1, 2 } → multi-element tuple
    pExprOrTuple = do
      exprs <- sepEndBy pExpr (symbol (pack ","))
      return $ ETuple exprs

-- Multiplicative operators
pOpMulDiv :: Parser Op
pOpMulDiv = (Mul <$ symbol (pack "*")) <|> (Div <$ symbol (pack "/"))

-- Additive operators (including string concatenation)
pOpAddSub :: Parser Op
pOpAddSub =
  try (Concat <$ symbol (pack "++"))  -- Must try ++ before +
  <|> (Add <$ symbol (pack "+"))
  <|> (Sub <$ symbol (pack "-"))

-- Comparison operators
pOpComparison :: Parser Op
pOpComparison =
  try (Lte <$ symbol (pack "<="))
  <|> try (Gte <$ symbol (pack ">="))
  <|> try (Eq <$ symbol (pack "=="))
  <|> try (Neq <$ symbol (pack "!="))
  <|> try (Lt <$ (symbol (pack "<") <* notFollowedBy (char '-')))  -- Don't match <- as <
  <|> (Gt <$ symbol (pack ">"))

-- Logical operators
pOpLogicalAnd :: Parser Op
pOpLogicalAnd = And <$ symbol (pack "&&")

pOpLogicalOr :: Parser Op
pOpLogicalOr = Or <$ symbol (pack "||")

-- Generic left-associative infix builder
makeInfixL :: Parser Expr -> Parser Op -> Parser Expr
makeInfixL termParser opParser = do
  term <- termParser
  DL.foldl' (\rec (op, nextTerm) -> EBinOp op rec nextTerm) term
    <$> many ((,) <$> opParser <*> termParser)

-- Multiplicative expressions
pExprMultiplicative :: Parser Expr
pExprMultiplicative = makeInfixL pPostfixExpr pOpMulDiv

-- Additive expressions (including string concatenation)
pExprAdditive :: Parser Expr
pExprAdditive = makeInfixL pExprMultiplicative pOpAddSub

-- Comparison expressions
pExprComparison :: Parser Expr
pExprComparison = makeInfixL pExprAdditive pOpComparison

-- Logical AND expressions
pExprLogicalAnd :: Parser Expr
pExprLogicalAnd = makeInfixL pExprComparison pOpLogicalAnd

-- Logical OR expressions
pExprLogicalOr :: Parser Expr
pExprLogicalOr = makeInfixL pExprLogicalAnd pOpLogicalOr

-- Send operator (right-associative)
pExprSend :: Parser Expr
pExprSend = do
  left <- pExprLogicalOr
  maybeOp <- optional (try (ESend <$ symbol (pack "<-")))
  case maybeOp of
    Nothing -> return left
    Just op -> op left <$> pExprSend

-- Postfix expressions (for field access)
pPostfixExpr :: Parser Expr
pPostfixExpr = do
  base <- pBaseExpr
  fields <- many (symbol (pack ".") *> pIdentifier)
  return $ DL.foldl' EFieldAccess base fields

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
    <|> try pIf
    <|> try pReceive
    <|> pSelf
    <|> pVarOrCall
    <|> pBraceContent
    <|> pParens

-- If-then-else expression
pIf :: Parser Expr
pIf = do
  _ <- symbol (pack "if")
  condition <- pExpr
  _ <- symbol (pack "then")
  thenBranch <- pExpr
  elseBranch <- optional (symbol (pack "else") *> pExpr)
  return $ EIf condition thenBranch elseBranch

-- Receive expression
-- Note: This uses a local copy of pattern parsing to avoid circular dependency
pReceive :: Parser Expr
pReceive = do
  _ <- symbol (pack "receive")
  cases <- between (symbol (pack "{")) (symbol (pack "}")) (many pReceiveCaseLocal)
  return $ EReceive cases
  where
    -- Local receive case parser
    pReceiveCaseLocal = do
      _ <- symbol (pack "|")
      pat <- pPatternLocal
      _ <- symbol (pack "->")
      expr <- pExpr
      return $ Case pat expr

    -- Local pattern parser to avoid circular import
    pPatternLocal =
      try pVarargsLocal
        <|> try (PLiteral <$> pLiteral)
        <|> try (pAtom >>= \expr -> case expr of
            EAtom a -> return (PAtom a)
            _ -> fail "Expected atom in pattern")
        <|> try (PTuple <$> between (symbol (pack "{")) (symbol (pack "}")) (sepEndBy pPatternLocal (symbol (pack ","))))
        <|> (PWildcard <$ symbol (pack "_"))
        <|> (PVar <$> pIdentifier)

    pVarargsLocal = do
      name <- pIdentifier
      _ <- symbol (pack "...")
      return $ PVarargs name

-- Self keyword
pSelf :: Parser Expr
pSelf = ESelf <$ symbol (pack "self")

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
