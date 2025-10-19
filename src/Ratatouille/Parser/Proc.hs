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
import Data.Text (pack)
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
import Ratatouille.Parser.ExprStmt (pExpr, pStatement)
import Ratatouille.Parser.Pattern (pReceiveCase)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    between,
    optional,
    sepEndBy,
    (<|>),
  )

-- Parse process body: { state: ..., receive { ... } }
pProcBody :: Parser ProcBody
pProcBody = between (symbol (pack "{")) (symbol (pack "}")) $ do
  maybeState <- optional (try (symbol (pack "state") *> symbol (pack ":") *> pExpr))
  _ <- optional (symbol (pack ","))
  maybeReceive <- optional (symbol (pack "receive") *> between (symbol (pack "{")) (symbol (pack "}")) (sepEndBy pReceiveCase (symbol (pack ","))))
  return $ ProcBody maybeState (fromMaybe [] maybeReceive)

-- Process definition: proc Name(params) { body }
pProcDef :: Parser ProcDefinition
pProcDef = do
  _ <- symbol (pack "proc")
  name <- pIdentifier
  params <- between (symbol (pack "(")) (symbol (pack ")")) (sepEndBy pIdentifier (symbol (pack ",")))
  ProcDef name params <$> pProcBody

-- Top-level definition: process or statement
pDefinition :: Parser Definition
pDefinition = (DProc <$> pProcDef) <|> (DStmt <$> pStatement)

-- Program: sequence of definitions separated by semicolons
pProgram :: Parser Program
pProgram = Program <$> (sc *> sepEndBy pDefinition (symbol (pack ";")) <* eof)
