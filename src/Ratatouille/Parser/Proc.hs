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
    pImport,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Ratatouille.AST
  ( Definition (..),
    ImportDecl (..),
    ImportItems (..),
    Literal (..),
    ProcBody (ProcBody),
    ProcDefinition (ProcDef),
    Program (..),
  )
import Ratatouille.Parser.Common
  ( Parser,
    pIdentifier,
    pStringLiteral,
    sc,
    symbol,
  )
import Ratatouille.Parser.ExprStmt (pExpr, pTopLevelStatement)
import Ratatouille.Parser.Pattern (pReceiveCase)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    between,
    lookAhead,
    many,
    optional,
    sepBy1,
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

-- Top-level definition: import, process, or top-level statement
pDefinition :: Parser Definition
pDefinition = 
  (DImport <$> pImport) <|>
  (DProc <$> pProcDef) <|> 
  (DStmt <$> pTopLevelStatement)

-- Parse an import declaration
-- Supports:
--   import "path/to/module.rat"                  -- ImportAll
--   import Counter from "module.rat"             -- ImportSingle
--   import {Counter, Timer} from "module.rat"    -- ImportSelected
pImport :: Parser ImportDecl
pImport = do
  _ <- symbol (pack "import")
  items <- pImportItems
  pathLit <- case items of
    ImportAll -> pStringLiteral  -- import "path"
    _ -> do
      _ <- symbol (pack "from")
      pStringLiteral
  -- Extract text from LString
  path <- case pathLit of
    (LString text) -> return text
    _ -> fail "Expected string literal for import path"
  return $ ImportDecl path items
  where
    pImportItems =
      -- Try ImportAll first (just a string literal)
      (try (ImportAll <$ lookAhead pStringLiteral)) <|>
      -- Try ImportSelected {A, B, C}
      (try $ do
        _ <- symbol (pack "{")
        items <- sepBy1 pIdentifier (symbol (pack ","))
        _ <- symbol (pack "}")
        return $ ImportSelected items) <|>
      -- Otherwise ImportSingle
      (ImportSingle <$> pIdentifier)

-- Program: sequence of definitions with optional semicolons
pProgram :: Parser Program
pProgram = Program <$> (sc *> many (pDefinition <* optional (symbol (pack ";"))) <* eof)
