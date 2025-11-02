# Ratatouille Language Architecture

**Project**: GLaDOS (Generic Language And Data Operand Syntax)
**Language**: Ratatouille
**Version**: 3.0.0
**Date**: November 2025

---

## Table of Contents

1. [Overview](#overview)
2. [System Architecture](#system-architecture)
3. [Compilation Pipeline](#compilation-pipeline)
4. [Parser Architecture](#parser-architecture)
5. [Abstract Syntax Tree (AST)](#abstract-syntax-tree-ast)
6. [Bytecode Architecture](#bytecode-architecture)
7. [Virtual Machine Architecture](#virtual-machine-architecture)
8. [Runtime System](#runtime-system)
9. [Type System](#type-system)
10. [Actor Model Implementation](#actor-model-implementation)
11. [Error Handling](#error-handling)
12. [Design Decisions and Trade-offs](#design-decisions-and-trade-offs)

---

## Overview

### What is Ratatouille?

Ratatouille is an **actor-model programming language** with a focus on:
- **Process-based concurrency** (inspired by Erlang/Elixir)
- **Strong static typing** (inspired by Rust/Haskell)
- **Memory safety** through managed runtime
- **Pattern matching** for message handling
- **Functional and imperative** programming paradigms

### System Components

```
┌─────────────────────────────────────────────────────────────────┐
│                        User Source Code                          │
│                         (.rat files)                             │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│                     COMPILER (glados)                            │
│                                                                   │
│  ┌──────────┐    ┌──────┐    ┌──────────┐    ┌─────────┐       │
│  │  Parser  │ -> │ AST  │ -> │ Compiler │ -> │ Encoder │       │
│  └──────────┘    └──────┘    └──────────┘    └─────────┘       │
│                                                                   │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Bytecode (.rtbc file)                         │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│                   VIRTUAL MACHINE (glados-vm)                    │
│                                                                   │
│  ┌─────────┐    ┌──────────────┐    ┌─────────────┐            │
│  │ Decoder │ -> │ Interpreter  │ -> │   Runtime   │            │
│  └─────────┘    └──────────────┘    └─────────────┘            │
│                                                                   │
│  ┌──────────────────────────────────────────────────────┐       │
│  │          Haskell Runtime (GHC/Stack)                 │       │
│  │  • Garbage Collector                                 │       │
│  │  • STM (Software Transactional Memory)               │       │
│  │  • Memory Safety                                     │       │
│  └──────────────────────────────────────────────────────┘       │
└─────────────────────────────────────────────────────────────────┘
```

---

## System Architecture

### Architectural Layers

The system is organized into **four distinct layers**:

#### 1. Language Layer (Source Code)
- **Responsibility**: User-written Ratatouille programs
- **Format**: `.rat` text files
- **Features**: Process definitions, expressions, pattern matching

#### 2. Compiler Layer
- **Responsibility**: Transform source code into executable bytecode
- **Components**:
  - **Parser**: Megaparsec-based parser with operator precedence
  - **AST**: Abstract Syntax Tree representation
  - **Compiler**: AST to bytecode transformation
  - **Encoder**: Binary serialization

#### 3. Virtual Machine Layer
- **Responsibility**: Execute bytecode in isolated processes
- **Components**:
  - **Decoder**: Binary deserialization
  - **Interpreter**: Stack-based bytecode execution
  - **Runtime**: Process management and message passing

#### 4. Platform Layer (Haskell Runtime)
- **Responsibility**: Memory management, concurrency primitives
- **Provided by**: GHC (Glasgow Haskell Compiler) runtime
- **Features**: Garbage collection, STM, type safety

### Separation of Concerns

| Layer | Language | Responsibility | Safe? |
|-------|----------|----------------|-------|
| Source | Ratatouille | Business logic | ✅ Type-checked |
| Compiler | Haskell | Code generation | ✅ Pure functions |
| Bytecode | Binary | Portable format | ✅ Validated |
| VM | Haskell | Execution | ✅ Memory safe |
| Runtime | Haskell/C | OS interface | ✅ GHC guarantees |

---

## Compilation Pipeline

### End-to-End Flow

```
Source.rat
    │
    │ (1) Lexing & Parsing
    ▼
  AST (Abstract Syntax Tree)
    │
    │ (2) Semantic Analysis (basic type checking)
    ▼
  Validated AST
    │
    │ (3) Bytecode Generation
    ▼
  Bytecode Instructions
    │
    │ (4) Encoding
    ▼
  Binary.rtbc
    │
    │ (5) Decoding
    ▼
  Bytecode Instructions (in VM)
    │
    │ (6) Interpretation
    ▼
  Execution (Process Creation, Message Passing)
```

### Detailed Pipeline Stages

#### Stage 1: Lexing & Parsing

**Location**: `src/Ratatouille/Parser/`

**Input**: Source code text (UTF-8)
**Output**: Abstract Syntax Tree (AST)

**Components**:
- `Common.hs`: Lexical tokens (identifiers, literals, keywords)
- `ExprStmt.hs`: Expression and statement parsing with operator precedence
- `Proc.hs`: Process and function definition parsing
- `Pattern.hs`: Pattern matching syntax

**Key Features**:
- **Operator Precedence Climbing**: 9 precedence levels (assignment to postfix)
- **Megaparsec Combinators**: Composable, error-recovery capable
- **Context-Sensitive Parsing**: Distinguishes pure functions from actor processes
- **Comment Handling**: Line (`//`) and block (`/* */`) comments

**Example**:
```ratatouille
proc Counter(init) {
  state: init,
  receive {
    | :inc -> state = state + 1
  }
}
```
↓ Parsed to:
```haskell
DProc (ProcDef "Counter" ["init"]
  (ProcBody
    (Just (EVar "init"))  -- state initialization
    [Case (PAtom "inc") (EAssign "state" (EBinOp Add ...))]
  )
)
```

#### Stage 2: Semantic Analysis

**Location**: Minimal (integrated into compiler)

**Checks**:
- ✅ Main entry point exists (`proc main()`)
- ✅ Variables declared before use (runtime check)
- ✅ Type annotations match (runtime validation)
- ⚠️ Pattern exhaustiveness (warnings only)

**Note**: Ratatouille favors simplicity over complex static analysis. Most checks deferred to runtime.

#### Stage 3: Bytecode Generation

**Location**: `src/Ratatouille/Bytecode/Compiler.hs`

**Input**: AST
**Output**: List of bytecode instructions

**Process**:
1. **Process/Function Registration**: Build lookup table
2. **Expression Compilation**: Convert to stack operations
3. **Statement Compilation**: Generate variable bindings
4. **Pattern Matching**: Compile to conditional jumps
5. **Label Resolution**: Calculate jump offsets

**Compilation Strategies**:

| AST Node | Bytecode Strategy |
|----------|-------------------|
| Literals | `PUSH_INT`, `PUSH_STRING`, etc. |
| Variables | `LOAD_VAR`, `STORE_VAR` |
| Binary Ops | `left_code + right_code + OP` |
| If-Then-Else | Conditional jump with offset calculation |
| Function Call | `CALL_FUNCTION` with arguments |
| Spawn | `CREATE_INSTANCE` |
| Send | `SEND` (receiver, message) |
| Receive | `WAIT_MESSAGE` + pattern matching jumps |

**Example**:
```ratatouille
let x = 2 + 3
```
↓ Compiles to:
```
PUSH_INT 2
PUSH_INT 3
ADD
STORE_VAR "x"
```

#### Stage 4: Encoding

**Location**: `src/Ratatouille/Bytecode/Encoder.hs`

**Input**: Bytecode instructions
**Output**: Binary `.rtbc` file

**Format**:
```
┌─────────────────────────────────────┐
│         Header (8 bytes)            │
│  ┌─────────────────────────────┐   │
│  │ Magic Number (4 bytes)      │   │  "RTBC"
│  │ Version (2 bytes)           │   │  Major.Minor
│  │ Instruction Count (2 bytes) │   │  Number of instructions
│  └─────────────────────────────┘   │
├─────────────────────────────────────┤
│    Instruction Stream (variable)    │
│  ┌─────────────────────────────┐   │
│  │ Opcode (1 byte)             │   │
│  │ Operands (variable length)  │   │
│  │ ...                         │   │
│  └─────────────────────────────┘   │
└─────────────────────────────────────┘
```

**Features**:
- **Type Preservation**: Values encoded with type tags
- **Compact Representation**: Variable-length encoding
- **Platform Independent**: Big-endian, explicit sizes
- **Versioning**: Forward/backward compatibility support

#### Stage 5: Decoding

**Location**: `src/Ratatouille/Bytecode/Decoder.hs`

**Input**: Binary `.rtbc` file
**Output**: Bytecode instructions (in memory)

**Validation**:
- ✅ Magic number verification ("RTBC")
- ✅ Version compatibility check
- ✅ Instruction count matches
- ✅ Well-formed instructions

**Error Handling**: Invalid bytecode → parse error, VM refuses to execute

#### Stage 6: Interpretation

**Location**: `src/Ratatouille/VM/Interpreter.hs`

**Input**: Bytecode instructions
**Output**: Side effects (process creation, I/O, state changes)

**Execution Model**: Stack-based virtual machine

**Execution Loop**:
```haskell
executeLoop :: VMState -> IO VMState
executeLoop state =
  if pc >= length bytecode
    then return state
    else do
      let instruction = bytecode !! pc
      newState <- executeInstruction instruction state
      executeLoop newState
```

---

## Parser Architecture

### Parser Combinator Design

Ratatouille uses **Megaparsec**, a modern parser combinator library:

**Advantages**:
- ✅ Composable parsers (small functions combine into larger ones)
- ✅ Excellent error messages with context
- ✅ Built-in lexing support
- ✅ Operator precedence handling
- ✅ Backtracking with `try`

### Operator Precedence Implementation

**Strategy**: Precedence climbing with separate parsers per level

```haskell
-- Level 1: Assignment (lowest precedence)
pExprAssign = assignmentParser <|> pExprSend

-- Level 2: Send operator
pExprSend = receiver <- pExprLogicalOr
            [ "<-" message <- pExprSend ]?

-- Level 3: Logical OR
pExprLogicalOr = chainLeft pExprLogicalAnd (symbol "||")

-- ... continues through 9 levels
```

**Precedence Table** (lowest to highest):
1. Assignment `=`
2. Send `<-`
3. Logical OR `||`
4. Logical AND `&&`
5. Comparison `==`, `!=`, `<`, `>`, `<=`, `>=`
6. Additive `+`, `-`, `++`
7. Multiplicative `*`, `/`
7.5. Unary `!`, `-`, `+`
8. Postfix `.field`, `[index]`
9. Base expressions (literals, calls, etc.)

### Pattern Matching Parser

**Challenge**: Distinguish tuples from parenthesized patterns

**Solution**: Lookahead for comma

```haskell
pParenPattern = do
  firstPat <- pPattern
  optional (symbol ",") >>= \case
    Just _ -> do
      restPats <- sepEndBy pPattern (symbol ",")
      return $ PTuple (firstPat : restPats)  -- Tuple (≥2 elements)
    Nothing -> return firstPat               -- Parenthesized pattern
```

---

## Abstract Syntax Tree (AST)

### AST Design Principles

1. **Explicit Structure**: Every language construct has a corresponding AST node
2. **Type Safety**: Haskell's type system prevents malformed ASTs
3. **Composability**: Expressions nest naturally
4. **Information Preservation**: Types, names, structure all retained

### Core AST Types

**Location**: `src/Ratatouille/AST.hs` (1,149 lines)

#### Program Structure
```haskell
data Program = Program [Definition]

data Definition
  = DProc ProcDefinition      -- Process definition
  | DFunc FuncDefinition      -- Function definition
  | DStmt Stmt                -- Top-level statement
  | DImport ImportDecl        -- Import declaration
```

#### Expressions
```haskell
data Expr
  = ELiteral Literal
  | EVar Text
  | EAtom Text
  | EBinOp Op Expr Expr
  | EUnaryOp UnaryOp Expr
  | EIf Expr Expr (Maybe Expr)
  | ECall Text [Expr]
  | ESpawn Text [Expr]
  | ESend Expr Expr
  | EReceive [ReceiveCase]
  | EMatch Expr [MatchCase]
  | ETuple [Expr]
  | EArray [Expr]
  | EBlock [Stmt] Expr
  | ECast CastType Type Expr
  | EJust Expr | ENone
  | ELeft Expr | ERight Expr
  | ESelf
  | EPreInc Text | EPreDec Text
  | EPostInc Text | EPostDec Text
  | EFieldAccess Expr Text
  | EIndex Expr Expr
  | EAssign Text Expr
```

#### Statements
```haskell
data Stmt
  = SLet Text (Maybe Type) Expr       -- let x = expr
  | SConst Text (Maybe Type) Expr     -- let const x = expr
  | SLetPattern Pattern Expr          -- let [x, y] = expr
  | SAssign Text Expr                 -- x = expr
  | SExpr Expr                        -- expression as statement
```

#### Patterns
```haskell
data Pattern
  = PVar Text                         -- x
  | PVarTyped Text (Maybe Type) Bool  -- x<i32>, const x
  | PLiteral Literal                  -- 42, "text"
  | PAtom Text                        -- :ok
  | PTuple [Pattern]                  -- (a, b, c)
  | PArray [Pattern]                  -- [a, b, c]
  | PWildcard                         -- _
  | PVarargs Text                     -- items...
```

#### Types
```haskell
data Type
  = TNumeric NumericType
  | TString | TBool | TPid | TAtom
  | TNone | TVoid | TAny
  | TTuple [Type]
  | TArray Type (Maybe Int)           -- [T] or [T, N]
  | TMaybe Type                       -- T?
  | TEither Type Type                 -- T!U

data NumericType
  = I8 | I16 | I32 | I64
  | U8 | U16 | U32 | U64
  | F32 | F64
```

### AST Transformations

**Current**: AST → Bytecode (single pass)

**Future Possibilities**:
- AST → AST optimizations (constant folding, dead code elimination)
- AST → Type-checked AST (explicit type annotations)
- AST → Multiple backends (JVM, LLVM, JavaScript)

---

## Bytecode Architecture

### Instruction Set Design

**Philosophy**: Stack-based, RISC-like instruction set

**Total Instructions**: 65+ opcodes organized into families

### Instruction Families

| Family | Opcode Range | Count | Purpose |
|--------|--------------|-------|---------|
| Stack Operations | 0x01-0x0F | 9 | PUSH, POP, DUP |
| Variable Operations | 0x10-0x1F | 8 | LOAD, STORE, INC, DEC |
| Process State | 0x20-0x2F | 3 | INIT_STATE, GET_STATE, SET_STATE |
| Arithmetic | 0x30-0x3F | 4 | ADD, SUB, MUL, DIV |
| String Operations | 0x3A | 1 | CONCAT |
| Comparison | 0x40-0x4F | 6 | CMP_EQ, CMP_LT, etc. |
| Logical Operations | 0x50-0x5F | 3 | AND, OR, NOT |
| Unary Operations | 0x58 | 1 | NEGATE |
| Value Operations | 0x5A-0x5F | 3 | PUSH_NONE, PUSH_BOOL, GET_FIELD |
| Actor Model | 0x60-0x6F | 6 | DEFINE_PROCESS, SPAWN, SEND |
| Pattern Matching | 0x70-0x7F | 7 | MATCH_ATOM, MATCH_TUPLE, etc. |
| Process Control | 0x80-0x8F | 2 | WAIT_MESSAGE, EXIT_PROCESS |
| Type Casting | 0x90-0x9F | 3 | STATIC_CAST, REINTERPRET_CAST |
| Control Flow | 0xA0-0xAF | 6 | JUMP, CALL, RETURN, HALT |
| Function Operations | 0xB0-0xBF | 2 | DEFINE_FUNCTION, CALL_FUNCTION |
| Array Operations | 0xC0-0xCF | 2 | INDEX, ARRAY_LENGTH |
| Maybe/Either | 0xD0-0xDF | 5 | PUSH_JUST, MAYBE_BIND, etc. |

### Stack Machine Model

**Execution**: Instructions operate on an implicit stack

**Example**:
```
Expression: 2 + 3 * 4
Bytecode:
  PUSH_INT 2
  PUSH_INT 3
  PUSH_INT 4
  MUL           -- Stack: [2, 12]
  ADD           -- Stack: [14]
```

**Advantages**:
- ✅ Simple to implement
- ✅ Compact bytecode
- ✅ Easy to understand
- ✅ Well-tested model (JVM, Python VM, etc.)

**Disadvantages**:
- ⚠️ More instructions than register-based
- ⚠️ Stack management overhead
- ⚠️ Less efficient than native code

### Bytecode Example

**Source**:
```ratatouille
proc main() {
  let x = 10
  let y = x + 5
}
```

**Bytecode**:
```
PUSH_INT 10
STORE_VAR "x"
LOAD_VAR "x"
PUSH_INT 5
ADD
STORE_VAR "y"
HALT
```

---

## Virtual Machine Architecture

### VM State

**Location**: `src/Ratatouille/VM/VM.hs`

The VM maintains the following state:

```haskell
data VMState = VMState
  { stack :: [Value]                    -- Operand stack
  , globals :: Map Text Value           -- Global variables
  , locals :: Map Text Value            -- Local variables
  , programCounter :: Int               -- Current instruction index
  , bytecode :: [Instruction]           -- Instruction stream
  , labels :: Map Text Int              -- Jump targets
  , processDefs :: Map Text ProcessDef  -- Process definitions
  , functionDefs :: Map Text FunctionDef -- Function definitions
  , processes :: Map PID Process        -- Running processes
  , nextPID :: PID                      -- PID allocator
  , currentPID :: Maybe PID             -- Current process context
  }
```

### Instruction Execution

**Location**: `src/Ratatouille/VM/Interpreter.hs` (675 lines)

**Pattern**: Each instruction has a handler function

```haskell
executeInstruction :: Instruction -> VMState -> IO VMState
executeInstruction instruction state = case instruction of
  PUSH_INT n -> return $ state { stack = VInt n : stack state }
  ADD -> do
    let (b:a:rest) = stack state
    return $ state { stack = add a b : rest }
  LOAD_VAR name -> do
    let value = lookupVar name state
    return $ state { stack = value : stack state }
  -- ... 60+ more handlers
```

### Stack Operations

| Instruction | Stack Before | Stack After | Description |
|-------------|--------------|-------------|-------------|
| PUSH_INT 42 | `[]` | `[42]` | Push integer |
| ADD | `[3, 2, ...]` | `[5, ...]` | Pop 2, push sum |
| DUP | `[42, ...]` | `[42, 42, ...]` | Duplicate top |
| POP_N 2 | `[a, b, c, ...]` | `[c, ...]` | Pop N values |

### Variable Scopes

**Three scopes**:
1. **Global**: Shared across entire program
2. **Local**: Process/function local
3. **State**: Per-process mutable state

**Lookup Order**: Local → Global → Error

### Process Context Switching

When a process calls another process:
1. Save current locals
2. Create new locals for callee
3. Execute callee code
4. Restore caller locals
5. Push result to caller stack

---

## Runtime System

### Process Management

**Location**: `src/Ratatouille/VM/Runtime.hs` (231 lines)

#### Process Structure
```haskell
data Process = Process
  { processID :: PID
  , processCode :: [Instruction]
  , processLocals :: Map Text Value
  , processState :: Maybe Value
  , messageQueue :: TQueue Message
  }
```

#### Process Lifecycle

```
┌──────────────┐
│   Created    │  spawn Counter(0)
└──────┬───────┘
       │
       ▼
┌──────────────┐
│   Running    │  Executing bytecode
└──┬────────┬──┘
   │        │
   │        └─────────┐
   ▼                  ▼
┌──────────────┐  ┌──────────────┐
│   Waiting    │  │ Terminated   │
│ (for message)│  └──────────────┘
└──────┬───────┘
       │
       ▼ (message arrives)
┌──────────────┐
│   Running    │
└──────────────┘
```

### Message Passing

**Implementation**: STM (Software Transactional Memory)

```haskell
sendMessage :: PID -> Value -> VMState -> IO ()
sendMessage targetPID message state = do
  let targetProcess = processes state ! targetPID
  let queue = messageQueue targetProcess
  atomically $ writeTQueue queue message
```

**Message Delivery Guarantees**:
- ✅ **Atomic**: Message fully delivered or not at all
- ✅ **Ordered**: FIFO per sender-receiver pair
- ✅ **Isolated**: Copy semantics (no shared references)
- ⚠️ **Not Guaranteed**: No delivery acknowledgment (async)

### Sender Tracking

**Feature**: Receiver knows who sent the message

**Implementation**: Automatic `sender` local variable

```ratatouille
receive {
  | :ping -> sender <- :pong  -- sender is the PID that sent :ping
}
```

**Bytecode**: `WAIT_MESSAGE` automatically sets `sender` local

### Concurrency Model

**Strategy**: Cooperative multitasking (not preemptive)

**Scheduling**: Round-robin (future enhancement: priority queues)

**Blocking**: Only at `WAIT_MESSAGE` instruction

**No Data Races**: Message passing enforces isolation

---

## Type System

### Type Categories

#### 1. Primitive Types
- **Integers**: i8, i16, i32, i64, u8, u16, u32, u64
- **Floats**: f32, f64
- **Booleans**: true, false
- **Strings**: UTF-8 text (Haskell Text)
- **Atoms**: :atom_name (symbolic identifiers)

#### 2. Compound Types
- **Tuples**: `(T1, T2, ...)` (heterogeneous, ≥2 elements)
- **Arrays**: `[T]` (homogeneous, dynamic size)

#### 3. Special Types
- **Maybe**: `T?` (optional values)
- **Either**: `T!U` (error handling)
- **PID**: Process identifiers
- **None**: Unit/null type
- **Void**: No return value (functions only)
- **Any**: Top type (accepts anything)

### Type Checking Strategy

**Hybrid Approach**:
- ✅ **Compile-time**: Type annotations validated
- ✅ **Runtime**: Type casts checked, operations validated
- ⚠️ **Limited Inference**: Basic inference for literals

**Trade-off**: Fast compilation vs. strong guarantees

### Type Casting

**Three Cast Types**:

1. **Static Cast (`scast`)**: Safe, runtime-checked
   ```ratatouille
   let x<i64> = scast<i64>(42)  -- Validates conversion
   ```

2. **Reinterpret Cast (`rcast`)**: Unsafe, no checks
   ```ratatouille
   let bits<u32> = rcast<u32>(-1)  -- Bit reinterpretation
   ```

3. **Const Cast (`ccast`)**: Remove const qualifier
   ```ratatouille
   let mutable = ccast(constValue)
   ```

---

## Actor Model Implementation

### Process Isolation

**Principle**: Shared-nothing architecture

Each process has:
- ✅ **Isolated stack**: Cannot access other process stacks
- ✅ **Isolated locals**: Process-local variables
- ✅ **Isolated state**: Mutable state per process
- ✅ **Private mailbox**: Message queue (TQueue)

**Enforcement**: VM does not provide primitives to bypass isolation

### Message Semantics

**Copy-on-Send**: Messages are copied, not shared

```haskell
sendMessage pid msg = do
  let msgCopy = deepCopy msg  -- Conceptual (GC handles actual copying)
  atomically $ writeTQueue (processQueue pid) msgCopy
```

**Immutable Messages**: Sender cannot modify message after sending

### Pattern Matching in Receive

**Compilation Strategy**: Jump table with fallthrough

**Example**:
```ratatouille
receive {
  | :add -> state = state + 1
  | :mul -> state = state * 2
  | _ -> none
}
```

**Compiled Bytecode**:
```
WAIT_MESSAGE               -- Block until message arrives
DUP                        -- Preserve message for next pattern
MATCH_ATOM ":add" @skip1   -- If not :add, jump to skip1
  -- :add handler
  GET_STATE
  PUSH_INT 1
  ADD
  SET_STATE
  JUMP @end
skip1:
DUP
MATCH_ATOM ":mul" @skip2
  -- :mul handler
  GET_STATE
  PUSH_INT 2
  MUL
  SET_STATE
  JUMP @end
skip2:
  -- wildcard handler
  PUSH_NONE
end:
```

### Process Spawning

**Implementation**:
```haskell
CREATE_INSTANCE "Counter" [VInt 0]
  1. Look up "Counter" process definition
  2. Allocate new PID
  3. Create process structure with code and params
  4. Initialize locals with parameters
  5. Add to processes map
  6. Return PID
```

---

## Error Handling

### Error Philosophy

**No Exceptions**: Ratatouille uses explicit error values

**Error Representation**: Either type

```ratatouille
let result<string!i32> = parseNumber(input)
match result {
  | ok(value) -> value + 1
  | ko(err) -> 0
}
```

### Error Categories

#### 1. Parse Errors
- **When**: Compilation time
- **Handling**: Fail compilation, show error message
- **Examples**: Syntax errors, invalid tokens

#### 2. Compile Errors
- **When**: Compilation time
- **Handling**: Fail compilation
- **Examples**: Missing main(), invalid AST

#### 3. Runtime Errors
- **When**: Execution time
- **Handling**: Crash process (not entire VM)
- **Examples**: Division by zero, type mismatch, stack underflow

#### 4. User Errors (Either)
- **When**: Application logic
- **Handling**: Explicit pattern matching
- **Examples**: File not found, invalid input

### Error Recovery

**Process Level**: Process crashes don't affect others

**System Level**: VM can continue after process crash

**Future**: Supervisor trees (like Erlang OTP)

---

## Design Decisions and Trade-offs

### 1. Garbage Collection vs. Manual Memory Management

**Decision**: Use GC (Haskell runtime)

**Rationale**:
- ✅ Memory safety guaranteed
- ✅ No use-after-free, double-free
- ✅ Simpler mental model
- ⚠️ GC pauses (trade-off for safety)

**Alternative Considered**: Rust-style ownership (too complex)

---

### 2. Stack-Based vs. Register-Based VM

**Decision**: Stack-based

**Rationale**:
- ✅ Simpler implementation
- ✅ Compact bytecode
- ✅ Well-understood model
- ⚠️ More instructions than register-based

**Alternative Considered**: Register VM (Lua-style) - more complex

---

### 3. Static vs. Dynamic Typing

**Decision**: Hybrid (static with runtime validation)

**Rationale**:
- ✅ Type safety at compile-time where possible
- ✅ Flexibility for rapid development
- ✅ Explicit type annotations document intent
- ⚠️ Some errors only caught at runtime

**Alternative Considered**: Full static typing (Haskell-style) - too restrictive

---

### 4. Pure Functions vs. Mutable State

**Decision**: Both (pure functions + actor state)

**Rationale**:
- ✅ Pure functions for logic
- ✅ Mutable state for process state
- ✅ Best of both worlds
- ✅ State isolated to process

**Alternative Considered**: Pure-only (Haskell) - awkward for actors

---

### 5. Message Passing vs. Shared Memory

**Decision**: Message passing only

**Rationale**:
- ✅ No data races by design
- ✅ Process isolation
- ✅ Fault containment
- ⚠️ Copy overhead for large messages

**Alternative Considered**: Shared memory with locks - too error-prone

---

### 6. Eager vs. Lazy Evaluation

**Decision**: Eager (strict evaluation)

**Rationale**:
- ✅ Predictable performance
- ✅ Easier to reason about
- ✅ No space leaks from thunks
- ⚠️ Can't handle infinite structures

**Alternative Considered**: Lazy (Haskell) - unpredictable performance

---

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Variable lookup | O(log n) | Map-based |
| Stack push/pop | O(1) | List cons/decons |
| Message send | O(1) amortized | STM queue |
| Process spawn | O(1) | Allocate + insert |
| Pattern matching | O(p) | p = number of patterns |
| Garbage collection | O(heap size) | Stop-the-world |

### Space Complexity

| Structure | Space | Notes |
|-----------|-------|-------|
| Value | O(1) to O(n) | Depends on type |
| Process | O(code + locals + mailbox) | Isolated |
| Stack | O(depth) | Limited by recursion |
| Globals | O(variables) | Shared across program |

### Bottlenecks

1. **GC Pauses**: Can cause latency spikes
2. **Message Copying**: Large messages are expensive
3. **No TCO**: Deep recursion causes stack overflow
4. **Interpreted**: Slower than compiled native code

### Optimization Opportunities

**Future Enhancements**:
- [ ] Tail call optimization (TCO)
- [ ] JIT compilation (hot path detection)
- [ ] Message passing optimization (reference counting)
- [ ] Incremental GC (reduce pause times)
- [ ] LLVM backend (native code generation)

---

## Testing Architecture

### Test Organization

**Location**: `test/` (4,949 lines across 13 files)

#### Test Levels

1. **Unit Tests**: Individual components (parsers, compiler, VM)
2. **Integration Tests**: End-to-end (parse → compile → execute)
3. **Coverage Tests**: Ensure all code paths exercised

#### Test Frameworks

- **hspec**: Main testing framework
- **hspec-discover**: Automatic test discovery
- **HPC**: Coverage reporting

### Test Coverage

| Component | Test File | Lines | Coverage |
|-----------|-----------|-------|----------|
| Parser | ParserSpec.hs | 413 | Comprehensive |
| AST | ASTSpec.hs | 549 | Extensive |
| Bytecode | BytecodeSpec.hs | 821 | Comprehensive |
| VM | VMSpec.hs | 428 | Good |
| Interpreter | InterpreterSpec.hs | 982 | Comprehensive |
| Runtime | RuntimeSpec.hs | 390 | Good |
| Patterns | PatternParserSpec.hs | 178 | Good |
| Integration | IntegrationSpec.hs | 283 | Good |

**Total**: 4,949 lines of test code (more than source code!)

---

## Future Enhancements

### Short Term (Next Version)

- [ ] Tail call optimization (prevent stack overflow)
- [ ] Disassembler (bytecode → human-readable)
- [ ] File I/O operations
- [ ] Standard library (list operations, string manipulation)

### Medium Term

- [ ] Module system (imports/exports)
- [ ] Generic types (parametric polymorphism)
- [ ] Supervisor trees (fault tolerance)
- [ ] Hot code reloading

### Long Term

- [ ] JIT compilation
- [ ] LLVM backend
- [ ] Native code generation
- [ ] Distributed processes (across machines)
- [ ] Formal verification support

---

## Conclusion

Ratatouille's architecture is designed for:
- ✅ **Safety**: Memory safety, type safety, concurrency safety
- ✅ **Simplicity**: Easy to understand and extend
- ✅ **Modularity**: Clear separation of concerns
- ✅ **Testability**: Comprehensive test suite

The system successfully combines:
- **Erlang's** actor model and fault tolerance
- **Rust's** explicit safety and type system
- **Haskell's** functional purity and memory safety

Into a cohesive, approachable language suitable for concurrent systems programming.

---

**Document Version**: 1.0
**Last Updated**: November 2025
**Maintainers**: GLaDOS Development Team

**Related Documentation**:
- [Security Analysis](SECURITY_ANALYSIS.md)
- [Language Syntax](ratatouille.ebnf)
