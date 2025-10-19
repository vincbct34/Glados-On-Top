{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- AST
-}

module Ratatouille.AST
  ( Program (..),
    Definition (..),
    ProcBody (..),
    ProcDefinition (..),
    ReceiveCase (..),
    Pattern (..),
    Expr (..),
    Literal (..),
    Op (..),
    Stmt (..),
  )
where

import Data.Text (Text)

-- | The top-level AST node representing a complete program.
-- The parser produces a Program containing a list of all top-level definitions
-- (procedures and statements) found in the source file.
newtype Program = Program [Definition]
  deriving (Show, Eq)

-- | A top-level definition in the program.
-- The parser creates DProc when it encounters a procedure/function declaration,
-- and DStmt for standalone statements at the top level.
data Definition
  = DProc ProcDefinition
  | DStmt Stmt
  deriving (Show, Eq)

-- | A procedure (or function) definition with name, parameters, and body.
-- The parser extracts:
--   - procName: the identifier of the procedure
--   - procParams: the list of parameter names
--   - procBody: the implementation (state + receive cases)
data ProcDefinition = ProcDef
  { procName :: Text,
    procParams :: [Text],
    procBody :: ProcBody
  }
  deriving (Show, Eq)

-- | The body of a procedure, representing an actor-like process.
-- The parser builds this from constructs like:
--   state <expr>
--   receive
--     <pattern> -> <expr>
--     ...
-- procState: optional initial state expression for the process
-- procReceive: list of message-handling cases (pattern matching)
data ProcBody = ProcBody
  { procState :: Maybe Expr,
    procReceive :: [ReceiveCase]
  }
  deriving (Show, Eq)

-- | A single case in a receive block: pattern -> expression.
-- When a message matches the Pattern, the corresponding Expr is evaluated.
-- The parser creates one ReceiveCase for each pattern-expression pair in a receive block.
data ReceiveCase = Case Pattern Expr
  deriving (Show, Eq)

-- | Patterns used for matching in receive blocks and function arguments.
-- The parser converts concrete pattern syntax into these constructors:
--   - PVar: a variable name that binds the matched value (e.g., 'x')
--   - PWildcard: underscore '_', matches anything without binding
--   - PLiteral: matches a specific literal value (int or string)
--   - PAtom: an atom/tag used as a constant identifier
--   - PTuple: matches tuples by recursively matching each element
data Pattern
  = PVar Text
  | PWildcard
  | PLiteral Literal
  | PAtom Text
  | PTuple [Pattern]
  deriving (Show, Eq)

-- | Expressions: all computable values and operations in the language.
-- The parser maps concrete syntax to these constructors:
--   - EVar: variable reference (identifier lookup)
--   - ELiteral: integer or string literal value
--   - EAtom: atom/tag, treated as a constant (different from variables)
--   - ETuple: tuple expression with multiple sub-expressions
--   - ECall: function/procedure call, e.g., foo(a, b)
--   - ESpawn: spawn a new process/actor running the named procedure with arguments
--   - ESend: send a message (second expr) to a target (first expr)
--   - EAssign: assignment expression that returns the assigned value
--   - EBlock: a block of statements followed by a result expression
--   - EReceive: inline receive expression (pattern-matching on messages)
--   - EBinOp: binary operation (parser handles precedence/associativity before building AST)
data Expr
  = EVar Text
  | ELiteral Literal
  | EAtom Text
  | ETuple [Expr]
  | ECall Text [Expr]
  | ESpawn Text [Expr]
  | ESend Expr Expr
  | EAssign Text Expr
  | EBlock [Stmt] Expr
  | EReceive [ReceiveCase]
  | EBinOp Op Expr Expr
  deriving (Show, Eq)

-- | Literal values supported by the language.
-- The parser tokenizes and converts numeric/string tokens into these constructors.
data Literal
  = LInt Integer
  | LString Text
  deriving (Show, Eq)

-- | Binary operators.
-- The parser converts operator tokens (+, -, *, /) to these variants.
-- Operator precedence and associativity are resolved during parsing,
-- so the AST already has the correct structure.
data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

-- | Statements: side-effecting program steps.
-- The parser distinguishes these based on keywords and syntax:
--   - SLet: introduces a new binding (e.g., 'let x = 5')
--   - SAssign: updates an existing variable (e.g., 'x = 10')
--   - SExpr: an expression used as a statement (e.g., function call for side effects)
data Stmt
  = SLet Text Expr
  | SAssign Text Expr
  | SExpr Expr
  deriving (Show, Eq)
