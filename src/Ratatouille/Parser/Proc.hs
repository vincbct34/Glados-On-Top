{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Proc
-}

module Ratatouille.Parser.Proc
  ( pProcBody,
    pProcDef,
    pFuncDef,
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
    FuncDefinition (FuncDef),
    Program (..),
  )
import Ratatouille.Parser.Common
  ( Parser,
    pIdentifier,
    pStringLiteral,
    sc,
    symbol,
  )
import Ratatouille.Parser.ExprStmt (pExpr, pTopLevelStatement, pBlock)
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
import Data.List (find)

-- Parse process body: handles both pure functions and actor processes
-- Pure function: proc foo(x) { x + 1 }
--   → ProcBody (Just expr) []
-- Actor process: proc foo() { state: expr, receive { } }
--   → ProcBody (Just expr) [cases] or ProcBody Nothing [cases]
pProcBody :: Parser ProcBody
pProcBody = between (symbol (pack "{")) (symbol (pack "}")) $ do
  -- Try to parse as actor proc first (starts with state: or receive)
  result <- (try pActorProcBody) <|> pPureFuncBody
  return result
  where
    -- Actor proc: has state: and/or receive { }
    -- To disambiguate from pure functions, we require either:
    --   1. state: keyword explicitly present, OR
    --   2. receive keyword explicitly present
    pActorProcBody = do
      -- Try to parse state: first
      maybeState <- optional (try (symbol (pack "state") *> symbol (pack ":") *> pExpr))
      case maybeState of
        Just stateExpr -> do
          -- We found state:, so this is definitely an actor proc
          _ <- optional (symbol (pack ","))
          maybeReceive <- optional (symbol (pack "receive") *> between (symbol (pack "{")) (symbol (pack "}")) (many pReceiveCase))
          return $ ProcBody (Just stateExpr) (fromMaybe [] maybeReceive)
        Nothing -> do
          -- No state:, so we MUST have receive to be an actor proc
          _ <- try (symbol (pack "receive"))  -- This will fail if no receive, so the whole parser fails
          receiveBlock <- between (symbol (pack "{")) (symbol (pack "}")) (many pReceiveCase)
          return $ ProcBody Nothing receiveBlock

    -- Pure function: just an expression or block with statements
    -- Try block first (handles multiple statements), then fall back to pure expression
    pPureFuncBody = do
      expr <- pBlock <|> pExpr
      return $ ProcBody (Just expr) []

-- Process definition: proc Name(params) { body }
-- This now handles BOTH:
--   1. Pure functions: proc Foo(a, b) { expr }
--   2. Actor procs: proc Foo(a, b) { state: expr, receive { } }
-- Returns a ProcDefinition in both cases, with empty receive list for pure functions
pProcDef :: Parser ProcDefinition
pProcDef = do
  _ <- symbol (pack "proc")
  name <- pIdentifier
  params <- between (symbol (pack "(")) (symbol (pack ")")) pProcParams
  body <- pProcBody
  return $ ProcDef name params body

-- Parse procedure parameters: empty, void, or list of identifiers
pProcParams :: Parser [Text]
pProcParams = do
  -- Check for explicit void parameter
  voidParam <- optional (try (symbol (pack "void")))
  case voidParam of
    Just _ -> return []  -- void means no parameters
    Nothing -> sepEndBy pIdentifier (symbol (pack ","))  -- regular parameters

-- Function definition: fn Name(params) { body_expr }
-- Functions are pure: no state, no receive, just a single expression
-- Supports: fn add(a, b) or fn factorial(n) or fn main()
pFuncDef :: Parser FuncDefinition
pFuncDef = do
  _ <- symbol (pack "fn")
  name <- pIdentifier
  params <- between (symbol (pack "(")) (symbol (pack ")")) pFuncParams
  body <- pFuncBody
  return $ FuncDef name params body
  where
    -- Parse function body: { statements... expr }
    -- The body can contain statements followed by a final expression
    pFuncBody = between (symbol (pack "{")) (symbol (pack "}")) pExpr

-- Parse function parameters: empty or list of identifiers
pFuncParams :: Parser [Text]
pFuncParams = sepEndBy pIdentifier (symbol (pack ","))

-- Top-level definition: import, process (which handles pure functions too), or top-level statement
-- NOTE: proc syntax now handles BOTH pure functions and actor processes
--       - proc foo() { expr } → pure function
--       - proc foo() { state: expr, receive { } } → actor process
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
-- Validates that a 'proc main()' entry point exists
pProgram :: Parser Program
pProgram = do
  sc
  definitions <- many (pDefinition <* optional (symbol (pack ";")))
  eof

  -- Check for main entry point (proc main with no params and empty receive list)
  let mainFunc = find isMainFunc definitions
  case mainFunc of
    Nothing -> fail "Program must contain a 'proc main()' entry point"
    Just _ -> return $ Program definitions
  where
    isMainFunc :: Definition -> Bool
    isMainFunc (DProc (ProcDef name params (ProcBody _ receive))) =
      name == pack "main" && null params && null receive
    isMainFunc _ = False
