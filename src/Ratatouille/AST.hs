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
    ReceiveCase (..),
    Pattern (..),
    Expr (..),
    Literal (..),
    Op (..),
    Stmt (..),
  )
where

import Data.Text (Text)

-- | Représente le programme Ratatouille complet.
newtype Program = Program [Definition]
  deriving (Show, Eq)

-- | Une définition de haut niveau (par exemple, un processus).
data Definition
  = ProcDef Text [Text] ProcBody
  -- ... d'autres définitions pourraient venir ici plus tard (ex: fonctions globales)
  deriving (Show, Eq)

-- | Le corps d'un processus, contenant son état initial et ses règles de réception.
data ProcBody = ProcBody
  { -- | Expression pour l'état initial (optionnel)
    procState :: Maybe Expr,
    -- | Liste des cas de réception
    procReceive :: [ReceiveCase]
  }
  deriving (Show, Eq)

-- | Un cas dans un bloc 'receive'.
data ReceiveCase = Case Pattern Expr
  deriving (Show, Eq)

-- | Les patterns utilisés dans les blocs 'receive' pour matcher les messages.
data Pattern
  = -- | Capture la valeur dans une variable (e.g., 'msg')
    PVar Text
  | -- | Ignore la valeur (e.g., '_')
    PWildcard
  | -- | Match une valeur littérale exacte
    PLiteral Literal
  | -- | Match un atome (e.g., ':deposit')
    PAtom Text
  | -- | Déstructure un tuple (e.g., '{ :deposit, amount }')
    PTuple [Pattern]
  deriving (Show, Eq)

-- | Les expressions qui composent le corps du programme ou des processus.
data Expr
  = -- | Référence à une variable (e.g., 'logger_pid')
    EVar Text
  | -- | Une valeur littérale
    ELiteral Literal
  | -- | Un atome (e.g., ':increment')
    EAtom Text
  | -- | Un tuple (e.g., '{ :deposit, 50 }')
    ETuple [Expr]
  | -- | Création d'un processus (e.g., 'spawn Logger(0)')
    ESpawn Text [Expr]
  | -- | Envoi de message (e.g., 'pid <- msg')
    ESend Expr Expr
  | -- | Bloc d'instructions (e.g., '{ let x = 1; x + 1 }')
    EBlock [Stmt] Expr
  | -- | Bloc 'receive' (utilisé dans procBody)
    -- ... d'autres expressions viendront ici (conditions, opérations, lambdas, etc.)
    EReceive [ReceiveCase]
  | -- | Opération binaire (e.g., 'a + b')
    EBinOp Op Expr Expr
  deriving (Show, Eq)

-- | Les littéraux du langage (valeurs primitives).
data Literal
  = -- | Un nombre entier (e.g., 123, -45)
    LInt Integer
  | -- | Une chaîne de caractères (e.g., "Hello World")
    LString Text
  deriving (Show, Eq)

data Op
  = -- | Addition
    Add
  | -- | Soustraction
    Sub
  | -- | Multiplication
    Mul
  | -- | Division
    Div
  deriving (Show, Eq)

-- | Les instructions qui peuvent être dans un bloc.
data Stmt
  = -- | Déclaration de variable (e.g., 'let x = 10')
    SLet Text Expr
  | -- | Une expression utilisée comme instruction (e.g., 'pid <- msg')
    -- ... d'autres instructions impératives viendront ici (mutables, boucles)
    SExpr Expr
  deriving (Show, Eq)
