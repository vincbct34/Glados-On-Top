{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- AST
-}

module Ratatouille.AST
  ( Program (..),
    Definition (..),
    ImportDecl (..),
    ImportItems (..),
    ProcBody (..),
    ProcDefinition (..),
    ReceiveCase (..),
    MatchCase (..),
    Pattern (..),
    Expr (..),
    Literal (..),
    Op (..),
    UnaryOp (..),
    Stmt (..),
    Type (..),
    NumericType (..),
    CastType (..),
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
-- DStmt for standalone statements at the top level, and DImport for import declarations.
data Definition
  = DProc ProcDefinition
  | DStmt Stmt
  | DImport ImportDecl
  deriving (Show, Eq)

-- | An import declaration
-- Represents: import <items> from "<path>"
data ImportDecl = ImportDecl
  { importPath :: Text,           -- Path to the module file
    importItems :: ImportItems    -- What to import from the module
  }
  deriving (Show, Eq)

-- | What items to import from a module
data ImportItems
  = ImportAll                     -- import "module.rat" (all procs)
  | ImportSelected [Text]         -- import {A, B} from "module.rat"
  | ImportSingle Text             -- import Counter from "module.rat"
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
--   - PVarTyped: a typed variable with optional const qualifier
--   - PWildcard: underscore '_', matches anything without binding
--   - PLiteral: matches a specific literal value (int or string)
--   - PAtom: an atom/tag used as a constant identifier
--   - PTuple: matches tuples by recursively matching each element
--   - PArray: matches arrays by recursively matching each element
--   - PVarargs: variadic pattern that captures remaining elements (e.g., 'rest...')
data Pattern
  = PVar Text
  | PVarTyped Text (Maybe Type) Bool  -- name, optional type, is_const
  | PWildcard
  | PLiteral Literal
  | PAtom Text
  | PTuple [Pattern]
  | PArray [Pattern]       -- Array pattern [a, b, c]
  | PVarargs Text  -- Captures remaining tuple/array elements
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
--   - EIf: conditional expression with optional else branch
--   - EFieldAccess: field access on an expression (e.g., state.value)
--   - ESelf: reference to current process PID
--   - ECast: type casting (static or reinterpret cast)
data Expr
  = EVar Text
  | ELiteral Literal
  | EAtom Text
  | ETuple [Expr]
  | EArray [Expr]                -- Array literal [1, 2, 3] or [{expr}] for initialization
  | EIndex Expr Expr             -- Array indexing arr[idx]
  | ECall Text [Expr]
  | ESpawn Text [Expr]
  | ESend Expr Expr
  | EAssign Text Expr
  | EBlock [Stmt] Expr
  | EReceive [ReceiveCase]
  | EBinOp Op Expr Expr
  | EUnaryOp UnaryOp Expr        -- Unary operations: !, -, +
  | EIf Expr Expr (Maybe Expr)  -- condition, then-branch, optional else-branch
  | EFieldAccess Expr Text       -- expression.field
  | ESelf                        -- self keyword
  | ECast CastType Type Expr     -- cast type, target type, expression to cast
  | EJust Expr                   -- Maybe constructor: Just value
  | ENone                        -- Maybe constructor: None (absence of value)
  | ELeft Expr                   -- Either constructor: Left value (ko)
  | ERight Expr                  -- Either constructor: Right value (ok)
  | EMaybeBind Expr Expr         -- Maybe monad bind: m >>= f
  | EEitherBind Expr Expr        -- Either monad bind: m >>= f
  | EPreInc Text                 -- Pre-increment: ++x (increments then returns new value)
  | EPostInc Text                -- Post-increment: x++ (returns old value then increments)
  | EPreDec Text                 -- Pre-decrement: --x (decrements then returns new value)
  | EPostDec Text                -- Post-decrement: x-- (returns old value then decrements)
  | EMatch Expr [MatchCase]      -- Match expression: match expr { | pattern -> value }
  deriving (Show, Eq)

-- | A single case in a match expression
-- Similar to ReceiveCase but for general pattern matching
data MatchCase = MatchCase
  { matchPattern :: Pattern,
    matchExpr :: Expr
  }
  deriving (Show, Eq)

-- | Unary operators
data UnaryOp
  = UNot        -- Logical negation: !x
  | UNeg        -- Arithmetic negation: -x
  | UPlus       -- Unary plus: +x (mostly for symmetry)
  deriving (Show, Eq)

-- | Type of cast operation
data CastType
  = StaticCast      -- scast: safe conversion with validation (e.g., i32 to i64)
  | ReinterpretCast -- rcast: reinterpret bits as different type (unsafe)
  | ConstCast       -- ccast: remove const qualification
  deriving (Show, Eq)

-- | Literal values supported by the language.
-- The parser tokenizes and converts numeric/string tokens into these constructors.
-- Now supports typed numeric literals for precise control over integer and floating-point types.
data Literal
  = LInt Integer              -- Untyped integer (inferred or defaults to i32)
  | LTypedInt NumericType Integer  -- Explicitly typed integer (e.g., 42i8, 100u64)
  | LFloat Double             -- Untyped floating-point (inferred or defaults to f64)
  | LTypedFloat NumericType Double -- Explicitly typed float (e.g., 3.14f32)
  | LString Text              -- String literal
  | LBool Bool                -- Boolean literal (true/false)
  | LNone                     -- none value for absence/null
  deriving (Show, Eq)

-- | Numeric types for integers and floating-point numbers.
-- Provides explicit control over size and signedness.
data NumericType
  = I8   -- Signed 8-bit integer (-128 to 127)
  | I16  -- Signed 16-bit integer (-32,768 to 32,767)
  | I32  -- Signed 32-bit integer (default for integers)
  | I64  -- Signed 64-bit integer
  | U8   -- Unsigned 8-bit integer (0 to 255)
  | U16  -- Unsigned 16-bit integer (0 to 65,535)
  | U32  -- Unsigned 32-bit integer
  | U64  -- Unsigned 64-bit integer
  | F32  -- 32-bit floating-point (single precision)
  | F64  -- 64-bit floating-point (double precision, default for floats)
  deriving (Show, Eq)

-- | Type annotations for variables and expressions.
-- Used in let statements, function parameters, and type assertions.
data Type
  = TNumeric NumericType      -- Numeric type (i32, f64, etc.)
  | TString                   -- String type
  | TBool                     -- Boolean type (true/false)
  | TTuple [Type]             -- Tuple type with element types
  | TArray Type (Maybe Integer) -- Array type: [T] (dynamic) or [T, N] (fixed-size)
  | TMaybe Type               -- Maybe/Optional type: T? (e.g., i32?)
  | TEither Type Type         -- Either type: T!U (e.g., i32!string)
  | TAtom                     -- Atom/tag type
  | TPid                      -- Process ID type
  | TNone                     -- None/null type
  | TVoid                     -- Void/unit type (no value)
  | TAny                      -- Any type (for untyped code)
  deriving (Show, Eq)

-- | Binary operators.
-- The parser converts operator tokens (+, -, *, /, ==, !=, <, >, <=, >=, &&, ||, ++) to these variants.
-- Operator precedence and associativity are resolved during parsing,
-- so the AST already has the correct structure.
data Op
  = Add
  | Sub
  | Mul
  | Div
  | Concat     -- String concatenation (++)
  | Eq         -- Equality (==)
  | Neq        -- Not equal (!=)
  | Lt         -- Less than (<)
  | Gt         -- Greater than (>)
  | Lte        -- Less than or equal (<=)
  | Gte        -- Greater than or equal (>=)
  | And        -- Logical and (&&)
  | Or         -- Logical or (||)
  deriving (Show, Eq)

-- | Statements: side-effecting program steps.
-- The parser distinguishes these based on keywords and syntax:
--   - SLet: introduces a new binding (e.g., 'let x = 5' or 'let x<i32> = 5')
--   - SLetPattern: destructuring let (e.g., 'let [x, y] = tuple' or 'let (a, b) = pair')
--   - SConst: introduces an immutable binding (e.g., 'const x = 5')
--   - SAssign: updates an existing variable (e.g., 'x = 10')
--   - SExpr: an expression used as a statement (e.g., function call for side effects)
-- Type annotations are optional; when absent, types are inferred.
data Stmt
  = SLet Text (Maybe Type) Expr    -- variable name, optional type, value
  | SLetPattern Pattern Expr        -- destructuring pattern, value
  | SConst Text (Maybe Type) Expr  -- constant name, optional type, value (immutable)
  | SAssign Text Expr              -- variable name, new value
  | SExpr Expr                     -- expression as statement
  deriving (Show, Eq)
