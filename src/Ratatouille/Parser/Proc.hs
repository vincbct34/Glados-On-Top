{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Proc
-}

module Ratatouille.Parser.Proc
  ( pProcBody,
    pProcDef,
    pDefinition,
    pProgram,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Ratatouille.AST
  ( Definition (..),
    ProcBody (ProcBody),
    ProcDefinition (ProcDef),
    Program (..),
  )
import Ratatouille.Parser.Common
  ( Parser,
    pIdentifier,
    sc,
    symbol,
  )
import Ratatouille.Parser.ExprStmt (pExpr, pTopLevelStatement)
import Ratatouille.Parser.Pattern (pReceiveCase)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    between,
    many,
    optional,
    sepEndBy,
    (<|>),
  )

-- Parse process body: { state: ..., receive { ... } }
pProcBody :: Parser ProcBody
pProcBody = between (symbol (pack "{")) (symbol (pack "}")) $ do
  maybeState <- optional (try (symbol (pack "state") *> symbol (pack ":") *> pExpr))
  _ <- optional (symbol (pack ","))
  maybeReceive <- optional (symbol (pack "receive") *> between (symbol (pack "{")) (symbol (pack "}")) (many pReceiveCase))
  return $ ProcBody maybeState (fromMaybe [] maybeReceive)

-- Process definition: proc Name(params) { body }
-- Supports: proc Foo(a, b, c) or proc Foo(void) or proc Foo()
pProcDef :: Parser ProcDefinition
pProcDef = do
  _ <- symbol (pack "proc")
  name <- pIdentifier
  params <- between (symbol (pack "(")) (symbol (pack ")")) pProcParams
  ProcDef name params <$> pProcBody

-- Parse procedure parameters: empty, void, or list of identifiers
pProcParams :: Parser [Text]
pProcParams = do
  -- Check for explicit void parameter
  voidParam <- optional (try (symbol (pack "void")))
  case voidParam of
    Just _ -> return []  -- void means no parameters
    Nothing -> sepEndBy pIdentifier (symbol (pack ","))  -- regular parameters

-- Top-level definition: process or top-level statement (const not allowed at top level)
pDefinition :: Parser Definition
pDefinition = (DProc <$> pProcDef) <|> (DStmt <$> pTopLevelStatement)

-- Program: sequence of definitions with optional semicolons
pProgram :: Parser Program
pProgram = Program <$> (sc *> many (pDefinition <* optional (symbol (pack ";"))) <* eof)
