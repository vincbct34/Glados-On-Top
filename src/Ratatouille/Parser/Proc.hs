{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Process and function definition parsers
-}

module Ratatouille.Parser.Proc
  ( pProcBody
  , pProcDef
  , pFuncDef
  , pDefinition
  , pProgram
  , pImport
  )
where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Ratatouille.AST
  ( Definition (..)
  , Expr
  , FuncDefinition (FuncDef)
  , ImportDecl (..)
  , ImportItems (..)
  , Literal (..)
  , ProcBody (ProcBody)
  , ProcDefinition (ProcDef)
  , Program (..)
  , ReceiveCase
  )
import Ratatouille.Parser.Common
  ( Parser
  , pIdentifier
  , pStringLiteral
  , sc
  , symbol
  )
import Ratatouille.Parser.ExprStmt (pBlock, pExpr, pTopLevelStatement)
import Ratatouille.Parser.Pattern (pReceiveCase)
import Text.Megaparsec
  ( MonadParsec (eof, try)
  , between
  , lookAhead
  , many
  , optional
  , sepBy1
  , sepEndBy
  , (<|>)
  )

-- | Parse process body (pure function or actor process)
-- Pure function: proc foo(x) { x + 1 }
--   → ProcBody (Just expr) []
-- Actor process: proc foo() { state: expr, receive { } }
--   → ProcBody (Just expr) [cases] or ProcBody Nothing [cases]
pProcBody :: Parser ProcBody
pProcBody =
  between (symbol (pack "{")) (symbol (pack "}")) $
    try pActorProcBody <|> pPureFuncBody

-- | Parse actor process body (has state and/or receive)
pActorProcBody :: Parser ProcBody
pActorProcBody = do
  maybeState <- optional parseStateInit
  case maybeState of
    Just stateExpr -> procWithState stateExpr
    Nothing -> procWithoutState

-- | Parse state initialization
parseStateInit :: Parser Expr
parseStateInit =
  try (symbol (pack "state") *> symbol (pack ":") *> pExpr)

-- | Process with state defined
procWithState :: Expr -> Parser ProcBody
procWithState stateExpr = do
  _ <- optional (symbol (pack ","))
  maybeReceive <- optional parseReceiveBlock
  return $ ProcBody (Just stateExpr) (fromMaybe [] maybeReceive)

-- | Process without state
procWithoutState :: Parser ProcBody
procWithoutState = do
  _ <- try (symbol (pack "receive"))
  receiveBlock <- parseReceiveBlock
  return $ ProcBody Nothing receiveBlock

-- | Parse receive block
parseReceiveBlock :: Parser [ReceiveCase]
parseReceiveBlock =
  symbol (pack "receive")
    *> between
         (symbol (pack "{"))
         (symbol (pack "}"))
         (many pReceiveCase)

-- | Parse pure function body
pPureFuncBody :: Parser ProcBody
pPureFuncBody = do
  expr <- pBlock <|> pExpr
  return $ ProcBody (Just expr) []

-- | Parse process definition: proc Name(params) { body }
-- Handles both pure functions and actor processes
pProcDef :: Parser ProcDefinition
pProcDef = do
  _ <- symbol (pack "proc")
  name <- pIdentifier
  params <- between (symbol (pack "(")) (symbol (pack ")")) pProcParams
  body <- pProcBody
  return $ ProcDef name params body

-- | Parse procedure parameters (empty, void, or identifiers)
pProcParams :: Parser [Text]
pProcParams = do
  voidParam <- optional (try (symbol (pack "void")))
  case voidParam of
    Just _ -> return []
    Nothing -> sepEndBy pIdentifier (symbol (pack ","))

-- | Parse function definition: fn Name(params) { body_expr }
-- Pure functions: no state, no receive, just a single expression
pFuncDef :: Parser FuncDefinition
pFuncDef = do
  _ <- symbol (pack "fn")
  name <- pIdentifier
  params <- pFuncParamsEnclosed
  body <- pFuncBodyEnclosed
  return $ FuncDef name params body

-- | Parse function parameters enclosed in parentheses
pFuncParamsEnclosed :: Parser [Text]
pFuncParamsEnclosed =
  between (symbol (pack "(")) (symbol (pack ")")) pFuncParams

-- | Parse function body enclosed in braces
pFuncBodyEnclosed :: Parser Expr
pFuncBodyEnclosed =
  between (symbol (pack "{")) (symbol (pack "}")) pExpr

-- | Parse function parameters
pFuncParams :: Parser [Text]
pFuncParams = sepEndBy pIdentifier (symbol (pack ","))

-- | Parse top-level definition
-- Can be import, process, or statement
pDefinition :: Parser Definition
pDefinition =
  (DImport <$> pImport)
    <|> (DProc <$> pProcDef)
    <|> (DStmt <$> pTopLevelStatement)

-- | Parse an import declaration
-- Supports: import "path", import Name from "path",
--           import {A, B} from "path"
pImport :: Parser ImportDecl
pImport = do
  _ <- symbol (pack "import")
  items <- pImportItems
  pathLit <- parseImportPath items
  path <- extractPathText pathLit
  return $ ImportDecl path items

-- | Parse import path based on import items
parseImportPath :: ImportItems -> Parser Literal
parseImportPath ImportAll = pStringLiteral
parseImportPath _ = symbol (pack "from") *> pStringLiteral

-- | Extract text from string literal
extractPathText :: Literal -> Parser Text
extractPathText (LString text) = return text
extractPathText _ = fail "Expected string literal for import path"

-- | Parse import items specification
pImportItems :: Parser ImportItems
pImportItems =
  try (ImportAll <$ lookAhead pStringLiteral)
    <|> try pImportSelected
    <|> (ImportSingle <$> pIdentifier)

-- | Parse selected imports: {A, B, C}
pImportSelected :: Parser ImportItems
pImportSelected = do
  _ <- symbol (pack "{")
  items <- sepBy1 pIdentifier (symbol (pack ","))
  _ <- symbol (pack "}")
  return $ ImportSelected items

-- | Parse complete program with main entry point validation
pProgram :: Parser Program
pProgram = do
  sc
  definitions <- many (pDefinition <* optional (symbol (pack ";")))
  eof
  validateMainExists definitions

-- | Validate that main entry point exists
validateMainExists :: [Definition] -> Parser Program
validateMainExists definitions =
  case find isMainFunc definitions of
    Nothing -> fail "Program must contain a 'proc main()' entry point"
    Just _ -> return $ Program definitions

-- | Check if definition is the main entry point
isMainFunc :: Definition -> Bool
isMainFunc (DProc (ProcDef name params (ProcBody _ receive))) =
  name == pack "main" && null params && null receive
isMainFunc _ = False
