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

-- | Represents the complete Ratatouille program.
-- A program is a sequence of top-level definitions.
newtype Program = Program [Definition]
  deriving (Show, Eq)

-- | A top-level definition in a Ratatouille program.
-- It can be either a process definition or a global statement.
data Definition
  = DProc ProcDefinition -- A process definition
  | DStmt Stmt -- A global statement
  deriving (Show, Eq)

-- | Represents the complete definition of a process.
data ProcDefinition = ProcDef
  { procName :: Text, -- The name of the process
    procParams :: [Text], -- List of parameter names
    procBody :: ProcBody -- The body of the process
  }
  deriving (Show, Eq)

-- | The body of a process, containing its state and receive cases.
data ProcBody = ProcBody
  { procState :: Maybe Expr, -- Optional initial state expression
    procReceive :: [ReceiveCase] -- List of message receive cases
  }
  deriving (Show, Eq)

-- | A single case within a 'receive' block.
data ReceiveCase = Case Pattern Expr
  deriving (Show, Eq)

-- | Patterns used for message matching in 'receive' blocks.
data Pattern
  = PVar Text -- Matches and binds to a variable
  | PWildcard -- Matches any value, ignores it ('_')
  | PLiteral Literal -- Matches a literal value
  | PAtom Text -- Matches an atom
  | PTuple [Pattern] -- Matches a tuple of patterns
  deriving (Show, Eq)

-- | Represents an expression in Ratatouille.
data Expr
  = EVar Text -- A variable reference
  | ELiteral Literal -- A literal value (integer, string)
  | EAtom Text -- An atom (e.g., :ok)
  | ETuple [Expr] -- A tuple of expressions (e.g., {1, "hello"})
  | ECall Text [Expr] -- Function call (e.g., print(msg), spawn(Logger, []))
  | ESpawn Text [Expr] -- Process spawning (not implemented yet, but in AST)
  | ESend Expr Expr -- Message sending (pid <- message)
  | EAssign Text Expr -- Variable assignment as expression (e.g., state = state + 1)
  | EBlock [Stmt] Expr -- A block of statements with a final expression
  | EReceive [ReceiveCase] -- A receive block (not implemented yet, but in AST)
  | EBinOp Op Expr Expr -- A binary operation (e.g., 1 + 2)
  deriving (Show, Eq)

-- | Represents a literal value.
data Literal
  = LInt Integer -- An integer literal
  | LString Text -- A string literal
  deriving (Show, Eq)

-- | Represents a binary operator.
data Op
  = Add -- Addition (+)
  | Sub -- Subtraction (-)
  | Mul -- Multiplication (*)
  | Div -- Division (/)
  deriving (Show, Eq)

-- | Represents a statement.
data Stmt
  = SLet Text Expr -- 'let' binding (e.g., let x = 10)
  | SAssign Text Expr -- Variable assignment (e.g., state = state + 1)
  | SExpr Expr -- An expression treated as a statement (its value is ignored)
  deriving (Show, Eq)
