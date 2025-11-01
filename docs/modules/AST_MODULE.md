# AST Module Documentation

**Project**: GLaDOS/Ratatouille
**Version**: 3.0.0
**Last Updated**: November 2025

---

## Overview

The Abstract Syntax Tree (AST) module defines the data structures that represent Ratatouille programs after parsing. The AST serves as the bridge between the parser (source code → AST) and the compiler (AST → bytecode).

### Location

```
src/Ratatouille/AST.hs (285 lines)
```

### Key Principles

1. **Explicit Structure**: Every language construct has a dedicated type
2. **Type Safety**: Haskell's type system prevents malformed ASTs
3. **Composability**: Expressions nest naturally via recursive types
4. **Information Preservation**: Types, names, and structure all retained

---

## Architecture

### AST Hierarchy

```
Program
  └── [Definition]
        ├── DProc ProcDefinition
        ├── DFunc FuncDefinition
        ├── DStmt Stmt
        └── DImport ImportDecl
```

### Design Philosophy

The AST is **immutable** and **purely functional**:
- No in-place modification
- Transformations create new AST nodes
- Safe for concurrent compilation passes

---

## Core Types

### 1. Program

```haskell
newtype Program = Program [Definition]
```

**Purpose**: Top-level AST node representing complete program

**Example**:
```ratatouille
proc Counter(init) { ... }
let x = 10
```

**AST**:
```haskell
Program [DProc counterDef, DStmt (SLet "x" Nothing (ELiteral (LInt 10)))]
```

---

### 2. Definition

```haskell
data Definition
  = DProc ProcDefinition      -- Process/actor definition
  | DFunc FuncDefinition      -- Pure function definition
  | DStmt Stmt                -- Top-level statement
  | DImport ImportDecl        -- Import declaration
```

**Purpose**: Top-level program constructs

#### DProc - Process Definition

**Represents**: Actor-model processes with message passing

**Example**:
```ratatouille
proc Counter(initial) {
  state: initial,
  receive {
    | :increment -> state = state + 1
    | :get -> state
  }
}
```

**AST**:
```haskell
DProc (ProcDef
  "Counter"
  ["initial"]
  (ProcBody
    (Just (EVar "initial"))
    [Case (PAtom "increment") (EAssign "state" (EBinOp Add (EVar "state") (ELiteral (LInt 1)))),
     Case (PAtom "get") (EVar "state")]))
```

#### DFunc - Function Definition

**Represents**: Pure functions (no side effects)

**Example**:
```ratatouille
fn add(a<i32>, b<i32>) -> i32 {
  a + b
}
```

**AST**:
```haskell
DFunc (FuncDef
  "add"
  ["a", "b"]
  (EBinOp Add (EVar "a") (EVar "b")))
```

#### DStmt - Top-Level Statement

**Represents**: Global variable declarations or expressions

**Example**:
```ratatouille
let globalCount = 0
```

**AST**:
```haskell
DStmt (SLet "globalCount" Nothing (ELiteral (LInt 0)))
```

#### DImport - Import Declaration

**Represents**: Module imports

**Example**:
```ratatouille
import {Counter, Worker} from "utils.rat"
```

**AST**:
```haskell
DImport (ImportDecl "utils.rat" (ImportSelected ["Counter", "Worker"]))
```

---

### 3. Expressions

```haskell
data Expr
  = EVar Text                    -- Variable reference
  | ELiteral Literal             -- Literal value
  | EAtom Text                   -- Atom (:tag)
  | ETuple [Expr]                -- Tuple (e1, e2, ...)
  | EArray [Expr]                -- Array [e1, e2, ...]
  | EIndex Expr Expr             -- Array indexing arr[idx]
  | ECall Text [Expr]            -- Function call
  | ESpawn Text [Expr]           -- Spawn process
  | ESend Expr Expr              -- Send message
  | EAssign Text Expr            -- Assignment
  | EBlock [Stmt] Expr           -- Block with statements
  | EReceive [ReceiveCase]       -- Receive expression
  | EBinOp Op Expr Expr          -- Binary operation
  | EUnaryOp UnaryOp Expr        -- Unary operation
  | EIf Expr Expr (Maybe Expr)   -- Conditional
  | EFieldAccess Expr Text       -- Field access
  | ESelf                        -- Current process PID
  | ECast CastType Type Expr     -- Type casting
  | EJust Expr                   -- Maybe: Just value
  | ENone                        -- Maybe: None
  | ELeft Expr                   -- Either: Left (ko)
  | ERight Expr                  -- Either: Right (ok)
  | EMaybeBind Expr Expr         -- Maybe bind (>>=)
  | EEitherBind Expr Expr        -- Either bind (>>=)
  | EPreInc Text                 -- ++x
  | EPostInc Text                -- x++
  | EPreDec Text                 -- --x
  | EPostDec Text                -- x--
  | EMatch Expr [MatchCase]      -- Match expression
```

#### Expression Examples

##### Variables and Literals

```ratatouille
x                              // EVar "x"
42                             // ELiteral (LInt 42)
"hello"                        // ELiteral (LString "hello")
true                           // ELiteral (LBool True)
:ok                            // EAtom "ok"
```

##### Compound Expressions

**Tuples**:
```ratatouille
(1, 2, 3)                      // ETuple [ELiteral (LInt 1), ELiteral (LInt 2), ELiteral (LInt 3)]
(x, "text", true)              // ETuple [EVar "x", ELiteral (LString "text"), ELiteral (LBool True)]
```

**Arrays**:
```ratatouille
[1, 2, 3]                      // EArray [ELiteral (LInt 1), ELiteral (LInt 2), ELiteral (LInt 3)]
arr[0]                         // EIndex (EVar "arr") (ELiteral (LInt 0))
```

##### Operations

**Binary Operations**:
```ratatouille
a + b                          // EBinOp Add (EVar "a") (EVar "b")
x == y                         // EBinOp Eq (EVar "x") (EVar "y")
"hello" ++ " world"            // EBinOp Concat (ELiteral ...) (ELiteral ...)
```

**Unary Operations**:
```ratatouille
!flag                          // EUnaryOp UNot (EVar "flag")
-x                             // EUnaryOp UNeg (EVar "x")
```

**Increment/Decrement**:
```ratatouille
++counter                      // EPreInc "counter"
counter++                      // EPostInc "counter"
```

##### Control Flow

**Conditionals**:
```ratatouille
if x > 0 then :pos else :neg   // EIf (EBinOp Gt ...) (EAtom "pos") (Just (EAtom "neg"))
```

**Pattern Matching**:
```ratatouille
match value {
  | 0 -> :zero
  | x -> :other
}
```

**AST**:
```haskell
EMatch (EVar "value")
  [MatchCase (PLiteral (LInt 0)) (EAtom "zero"),
   MatchCase (PVar "x") (EAtom "other")]
```

##### Actor Model

**Spawning**:
```ratatouille
spawn Counter(0)               // ESpawn "Counter" [ELiteral (LInt 0)]
```

**Sending**:
```ratatouille
pid <- :increment              // ESend (EVar "pid") (EAtom "increment")
pid <- (:add, 5)               // ESend (EVar "pid") (ETuple [...])
```

**Receiving**:
```ratatouille
receive {
  | :ping -> :pong
  | x -> x + 1
}
```

**AST**:
```haskell
EReceive
  [Case (PAtom "ping") (EAtom "pong"),
   Case (PVar "x") (EBinOp Add (EVar "x") (ELiteral (LInt 1)))]
```

##### Type Casting

```ratatouille
scast<i64>(42)                 // ECast StaticCast (TNumeric I64) (ELiteral (LInt 42))
rcast<u32>(-1)                 // ECast ReinterpretCast (TNumeric U32) (EUnaryOp UNeg ...)
```

##### Maybe and Either

**Maybe**:
```ratatouille
just(42)                       // EJust (ELiteral (LInt 42))
none                           // ENone
value >>= fn                   // EMaybeBind (EVar "value") (EVar "fn")
```

**Either**:
```ratatouille
ok(result)                     // ERight (EVar "result")
ko("error")                    // ELeft (ELiteral (LString "error"))
result >>= handler             // EEitherBind (EVar "result") (EVar "handler")
```

---

### 4. Statements

```haskell
data Stmt
  = SLet Text (Maybe Type) Expr       -- let x = expr
  | SConst Text (Maybe Type) Expr     -- const x = expr
  | SLetPattern Pattern Expr          -- let (x, y) = expr
  | SAssign Text Expr                 -- x = expr
  | SExpr Expr                        -- expression statement
```

#### Statement Examples

##### Let Bindings

```ratatouille
let x = 10                     // SLet "x" Nothing (ELiteral (LInt 10))
let y<i32> = 42                // SLet "y" (Just (TNumeric I32)) (ELiteral (LInt 42))
```

##### Const Bindings

```ratatouille
const PI = 3.14159             // SConst "PI" Nothing (ELiteral (LFloat 3.14159))
```

##### Destructuring

```ratatouille
let (x, y) = (10, 20)          // SLetPattern (PTuple [PVar "x", PVar "y"]) (ETuple [...])
let [first, rest...] = list    // SLetPattern (PArray [PVar "first", PVarargs "rest"]) (EVar "list")
```

##### Assignment

```ratatouille
x = 42                         // SAssign "x" (ELiteral (LInt 42))
state = state + 1              // SAssign "state" (EBinOp Add ...)
```

##### Expression Statement

```ratatouille
print(x)                       // SExpr (ECall "print" [EVar "x"])
spawn Worker()                 // SExpr (ESpawn "Worker" [])
```

---

### 5. Patterns

```haskell
data Pattern
  = PVar Text                         -- x
  | PVarTyped Text (Maybe Type) Bool  -- x<i32>, const x
  | PWildcard                         -- _
  | PLiteral Literal                  -- 42, "text"
  | PAtom Text                        -- :ok
  | PTuple [Pattern]                  -- (a, b, c)
  | PArray [Pattern]                  -- [a, b, c]
  | PVarargs Text                     -- rest...
```

#### Pattern Examples

##### Variable Patterns

```ratatouille
| x -> ...                     // PVar "x"
| value<i32> -> ...            // PVarTyped "value" (Just (TNumeric I32)) False
| const name -> ...            // PVarTyped "name" Nothing True
```

##### Wildcard

```ratatouille
| _ -> ...                     // PWildcard
```

##### Literal Patterns

```ratatouille
| 0 -> ...                     // PLiteral (LInt 0)
| "hello" -> ...               // PLiteral (LString "hello")
| true -> ...                  // PLiteral (LBool True)
```

##### Atom Patterns

```ratatouille
| :ok -> ...                   // PAtom "ok"
| :error -> ...                // PAtom "error"
```

##### Tuple Patterns

```ratatouille
| (x, y) -> ...                // PTuple [PVar "x", PVar "y"]
| (:add, a, b) -> ...          // PTuple [PAtom "add", PVar "a", PVar "b"]
| (_, _, z) -> ...             // PTuple [PWildcard, PWildcard, PVar "z"]
```

##### Array Patterns

```ratatouille
| [x, y, z] -> ...             // PArray [PVar "x", PVar "y", PVar "z"]
| [first, rest...] -> ...      // PArray [PVar "first", PVarargs "rest"]
| [] -> ...                    // PArray []
```

---

### 6. Types

```haskell
data Type
  = TNumeric NumericType      -- i32, f64, etc.
  | TString                   -- string
  | TBool                     -- bool
  | TTuple [Type]             -- (T1, T2, ...)
  | TArray Type (Maybe Integer) -- [T] or [T, N]
  | TMaybe Type               -- T?
  | TEither Type Type         -- T!U
  | TAtom                     -- atom
  | TPid                      -- pid
  | TNone                     -- none
  | TVoid                     -- void
  | TAny                      -- any
```

#### Type Examples

##### Primitive Types

```ratatouille
i32                            // TNumeric I32
f64                            // TNumeric F64
string                         // TString
bool                           // TBool
pid                            // TPid
atom                           // TAtom
```

##### Numeric Types

```haskell
data NumericType
  = I8 | I16 | I32 | I64       -- Signed integers
  | U8 | U16 | U32 | U64       -- Unsigned integers
  | F32 | F64                  -- Floating-point
```

**Ranges**:
- `I8`: -128 to 127
- `U8`: 0 to 255
- `I32`: -2,147,483,648 to 2,147,483,647
- `U64`: 0 to 18,446,744,073,709,551,615
- `F32`: Single precision float
- `F64`: Double precision float

##### Compound Types

**Tuples**:
```ratatouille
(i32, string)                  // TTuple [TNumeric I32, TString]
(i32, i32, i32)                // TTuple [TNumeric I32, TNumeric I32, TNumeric I32]
```

**Arrays**:
```ratatouille
[i32]                          // TArray (TNumeric I32) Nothing (dynamic)
[string, 10]                   // TArray TString (Just 10) (fixed-size)
```

**Maybe (Optional)**:
```ratatouille
i32?                           // TMaybe (TNumeric I32)
string?                        // TMaybe TString
```

**Either (Result)**:
```ratatouille
i32!string                     // TEither (TNumeric I32) TString
bool!atom                      // TEither TBool TAtom
```

---

### 7. Operators

```haskell
data Op
  = Add | Sub | Mul | Div      -- Arithmetic
  | Concat                     -- String concatenation
  | Eq | Neq                   -- Equality
  | Lt | Gt | Lte | Gte        -- Comparison
  | And | Or                   -- Logical

data UnaryOp
  = UNot                       -- Logical negation
  | UNeg                       -- Arithmetic negation
  | UPlus                      -- Unary plus
```

---

### 8. Literals

```haskell
data Literal
  = LInt Integer              -- Untyped integer
  | LTypedInt NumericType Integer  -- Typed integer
  | LFloat Double             -- Untyped float
  | LTypedFloat NumericType Double -- Typed float
  | LString Text              -- String
  | LBool Bool                -- Boolean
  | LNone                     -- None/null
```

#### Literal Examples

```ratatouille
42                             // LInt 42
42i8                           // LTypedInt I8 42
100u64                         // LTypedInt U64 100
3.14                           // LFloat 3.14
2.5f32                         // LTypedFloat F32 2.5
"hello"                        // LString "hello"
true                           // LBool True
none                           // LNone
```

---

## AST Traversal

### Pattern: Recursive Descent

Most AST operations use **recursive descent**:

```haskell
-- Example: Count all variables in an expression
countVars :: Expr -> Int
countVars (EVar _) = 1
countVars (EBinOp _ e1 e2) = countVars e1 + countVars e2
countVars (EIf cond then_ else_) =
  countVars cond + countVars then_ + maybe 0 countVars else_
countVars (ETuple exprs) = sum (map countVars exprs)
-- ... other cases
```

### Pattern: Fold

**Purpose**: Accumulate results from traversal

```haskell
-- Example: Collect all function calls
collectCalls :: Expr -> [Text]
collectCalls (ECall name _) = [name]
collectCalls (EBinOp _ e1 e2) = collectCalls e1 ++ collectCalls e2
collectCalls (ETuple exprs) = concatMap collectCalls exprs
-- ... other cases
```

### Pattern: Transform

**Purpose**: Modify AST (create new nodes)

```haskell
-- Example: Replace all occurrences of a variable
replaceVar :: Text -> Expr -> Expr -> Expr
replaceVar old new (EVar name)
  | name == old = new
  | otherwise = EVar name
replaceVar old new (EBinOp op e1 e2) =
  EBinOp op (replaceVar old new e1) (replaceVar old new e2)
-- ... other cases
```

---

## AST Validation

### Well-Formedness Checks

The compiler performs validation during and after AST construction:

#### 1. Main Entry Point

```haskell
-- Must have a proc main()
validateMainExists :: Program -> Either String ()
```

#### 2. Variable Scoping

```haskell
-- Variables must be declared before use (checked at runtime)
validateVariableScopes :: Program -> Either String ()
```

#### 3. Type Consistency

```haskell
-- Type annotations must be consistent
validateTypes :: Expr -> Either String ()
```

#### 4. Pattern Exhaustiveness

```haskell
-- Warn if patterns are not exhaustive (warning only)
checkPatternExhaustiveness :: [Pattern] -> [String]
```

---

## AST Optimization (Future)

### Planned Optimizations

#### 1. Constant Folding

```haskell
-- Transform: 2 + 3  →  5
foldConstants :: Expr -> Expr
foldConstants (EBinOp Add (ELiteral (LInt a)) (ELiteral (LInt b))) =
  ELiteral (LInt (a + b))
```

#### 2. Dead Code Elimination

```haskell
-- Remove unreachable code after return/exit
eliminateDeadCode :: [Stmt] -> [Stmt]
```

#### 3. Inline Expansion

```haskell
-- Inline small functions at call sites
inlineFunction :: Text -> Expr -> Program -> Program
```

---

## Performance Characteristics

### Space Complexity

| AST Node | Space | Notes |
|----------|-------|-------|
| Literal | O(1) to O(n) | Depends on literal size |
| Variable | O(1) | Just a name reference |
| Binary Op | O(left + right) | Recursive structure |
| Tuple/Array | O(Σ elements) | Sum of element sizes |
| If-Then-Else | O(cond + then + else) | Three sub-expressions |

### Time Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| AST Construction | O(n) | n = source size |
| Traversal | O(nodes) | Visit each node once |
| Transformation | O(nodes) | Create new nodes |
| Validation | O(nodes) | Single pass |

---

## Design Patterns

### 1. Algebraic Data Types

**Benefit**: Exhaustive pattern matching enforced by compiler

```haskell
-- Compiler ensures all cases are handled
compileExpr :: Expr -> Bytecode
compileExpr expr = case expr of
  EVar name -> [LOAD_VAR name]
  ELiteral lit -> compileLiteral lit
  EBinOp op e1 e2 -> compileExpr e1 ++ compileExpr e2 ++ compileOp op
  -- ... must handle all constructors
```

### 2. Smart Constructors

**Purpose**: Create valid AST nodes with constraints

```haskell
-- Ensure tuples have at least 2 elements
mkTuple :: [Expr] -> Either String Expr
mkTuple [x] = Left "Tuple must have at least 2 elements"
mkTuple xs@(_:_:_) = Right (ETuple xs)
mkTuple [] = Left "Empty tuple"
```

### 3. Catamorphisms (Folds)

**Purpose**: Generic AST reduction

```haskell
-- Fold over expression structure
foldExpr :: (Expr -> a) -> (a -> a -> a) -> Expr -> a
```

---

## Testing Strategy

### Unit Tests

Test individual AST node construction:

```haskell
testLiteralConstruction = do
  let ast = ELiteral (LInt 42)
  assertEqual ast (ELiteral (LInt 42))
```

### Property Tests

Use QuickCheck for property-based testing:

```haskell
-- Property: Serialization roundtrip
prop_serializeRoundtrip :: Expr -> Bool
prop_serializeRoundtrip expr =
  (deserialize . serialize) expr == expr
```

---

## Best Practices

### 1. Use Type Annotations

Explicitly annotate types for documentation:

```haskell
compileExpr :: Expr -> Bytecode
```

### 2. Avoid Partial Functions

Use `Maybe` or `Either` instead of error:

```haskell
-- Good
lookupVar :: Text -> Env -> Maybe Value

-- Bad
lookupVar :: Text -> Env -> Value  -- Crashes if not found
```

### 3. Document Constructors

Add comments to explain each constructor:

```haskell
data Expr
  = EVar Text          -- Variable reference (identifier lookup)
  | ELiteral Literal   -- Constant literal value
  -- ...
```

---

## Related Documentation

- [Parser Module](PARSER_MODULE.md) - Source code → AST
- [Bytecode Compiler](BYTECODE_MODULE.md) - AST → Bytecode
- [Architecture Overview](ARCHITECTURE.md) - System architecture
- [Language Syntax](ratatouille.ebnf) - Formal grammar

---

**Document Version**: 1.0
**Maintainers**: GLaDOS Development Team
