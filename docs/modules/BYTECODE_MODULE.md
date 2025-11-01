# Bytecode Module Documentation

**Project**: GLaDOS/Ratatouille
**Version**: 3.0.0
**Last Updated**: November 2025

---

## Overview

The Bytecode module implements the compilation of AST (Abstract Syntax Tree) into stack-based bytecode instructions, and provides serialization/deserialization to binary format (`.rtbc` files). This module serves as the bridge between the high-level language and the virtual machine.

### Location

```
src/Ratatouille/Bytecode/
  ├── Types.hs      - Value and Instruction definitions
  ├── Compiler.hs   - AST → Bytecode compilation
  ├── Encoder.hs    - Bytecode → Binary serialization
  └── Decoder.hs    - Binary → Bytecode deserialization
```

### Key Responsibilities

1. **Compilation**: Transform AST into bytecode instructions
2. **Encoding**: Serialize bytecode to binary format
3. **Decoding**: Deserialize binary format back to bytecode
4. **Validation**: Ensure bytecode correctness

---

## Architecture

### Compilation Pipeline

```
AST → Compiler → [Instruction] → Encoder → Binary (.rtbc) → Decoder → [Instruction] → VM
```

### Design Philosophy

- **Stack-Based**: All operations manipulate an implicit stack
- **RISC-Like**: Simple, orthogonal instructions
- **Portable**: Platform-independent binary format
- **Versioned**: Support for forward/backward compatibility

---

## Types.hs - Core Definitions

### Value Types

```haskell
data Value
  = VInt Integer          -- Integer value
  | VFloat Double         -- Floating-point value
  | VString Text          -- String value
  | VAtom Text            -- Atom/symbol (:tag)
  | VTuple [Value]        -- Tuple (multiple values)
  | VArray [Value]        -- Array (homogeneous list)
  | VPid Integer          -- Process identifier
  | VUnit                 -- Unit/void value
  | VNone                 -- None/null value
  | VBool Bool            -- Boolean value
  | VJust Value           -- Maybe: Just x
  | VLeft Value           -- Either: Left x (error/ko)
  | VRight Value          -- Either: Right x (success/ok)
```

**Purpose**: Runtime values manipulated by the VM

**Examples**:
```haskell
VInt 42                            -- Integer 42
VString "hello"                    -- String "hello"
VTuple [VInt 10, VInt 20]          -- Tuple (10, 20)
VArray [VInt 1, VInt 2, VInt 3]    -- Array [1, 2, 3]
VJust (VInt 42)                    -- Maybe value: just(42)
VRight (VString "ok")              -- Either success: ok("ok")
```

### Instruction Set

```haskell
data Instruction
  = -- Stack operations (0x01-0x0F)
    PUSH_INT Integer
  | PUSH_FLOAT Double
  | PUSH_STRING Text
  | PUSH_ATOM Text
  | PUSH_TUPLE Int        -- Pop N elements, create tuple
  | PUSH_ARRAY Int        -- Pop N elements, create array
  | PUSH_UNIT             -- Push unit value
  | POP_N Int             -- Pop N elements
  | DUP                   -- Duplicate top of stack

  -- Variable operations (0x10-0x1F)
  | LOAD_VAR Text         -- Load global variable
  | STORE_VAR Text        -- Store global variable
  | LOAD_LOCAL Text       -- Load local variable
  | STORE_LOCAL Text      -- Store local variable

  -- Array operations (0x14-0x15)
  | INDEX                 -- arr[idx]: pop idx, arr, push arr[idx]
  | ARRAY_LENGTH          -- Pop array, push length

  -- Process state (0x20-0x2F)
  | INIT_STATE            -- Initialize process state
  | GET_STATE             -- Push process state
  | SET_STATE             -- Set process state

  -- Arithmetic (0x30-0x3F)
  | ADD | SUB | MUL | DIV
  | CONCAT                -- String concatenation

  -- Increment/Decrement (0x35-0x38)
  | INC_VAR Text          -- ++x: increment then push
  | DEC_VAR Text          -- --x: decrement then push
  | INC_VAR_POST Text     -- x++: push then increment
  | DEC_VAR_POST Text     -- x--: push then decrement

  -- Comparison (0x40-0x4F)
  | CMP_EQ | CMP_NEQ
  | CMP_LT | CMP_GT | CMP_LTE | CMP_GTE

  -- Logical (0x46-0x49)
  | LOGIC_AND | LOGIC_OR | LOGIC_NOT
  | NEGATE                -- Arithmetic negation

  -- Value operations (0x50-0x5F)
  | PUSH_NONE
  | PUSH_BOOL Bool
  | GET_FIELD Text        -- Get field from tuple/record

  -- Maybe/Either (0x53-0x57)
  | PUSH_JUST             -- Wrap top in Just
  | PUSH_LEFT             -- Wrap top in Left (ko)
  | PUSH_RIGHT            -- Wrap top in Right (ok)
  | MAYBE_BIND Text       -- Maybe monad bind
  | EITHER_BIND Text      -- Either monad bind

  -- Actor model (0x60-0x6F)
  | DEFINE_PROCESS Text [Text] Bytecode
  | CREATE_INSTANCE Text Int   -- Spawn process
  | SEND                       -- Send message
  | WAIT_MESSAGE               -- Wait for message

  -- Functions (0x64-0x65)
  | DEFINE_FUNCTION Text [Text] Bytecode
  | CALL_FUNCTION Text Int

  -- Pattern matching (0x70-0x7F)
  | MATCH_ATOM Text Int        -- Match atom, jump if no match
  | MATCH_VAR Text             -- Bind variable
  | MATCH_TUPLE Int Int        -- Match tuple size, jump if no match
  | MATCH_WILDCARD             -- Always matches
  | MATCH_INT Int Int          -- Match integer, jump if no match
  | MATCH_BOOL Bool Int        -- Match boolean, jump if no match
  | MATCH_STRING Text Int      -- Match string, jump if no match

  -- Process control (0x80-0x8F)
  | PROCESS_LOOP               -- Main process message loop
  | SELF                       -- Push current PID
  | EXIT_PROCESS               -- Terminate process

  -- Type casting (0x90-0x9F)
  | STATIC_CAST Text           -- Safe cast
  | REINTERPRET_CAST Text      -- Unsafe cast
  | CONST_CAST                 -- Remove const

  -- Control flow (0xA0-0xAF)
  | JUMP Int                   -- Unconditional jump (offset)
  | JUMP_IF_FALSE Int          -- Conditional jump
  | LABEL Text                 -- Jump target label
  | CALL Text                  -- Call function
  | RETURN                     -- Return from function
  | PRINT                      -- Print value (debug)
  | HALT                       -- Stop execution
```

### Opcode Ranges

| Range | Category | Count |
|-------|----------|-------|
| 0x01-0x0F | Stack operations | 9 |
| 0x10-0x1F | Variables | 8 |
| 0x20-0x2F | Process state | 3 |
| 0x30-0x3F | Arithmetic | 9 |
| 0x40-0x4F | Comparison | 6 |
| 0x50-0x5F | Value operations | 6 |
| 0x60-0x6F | Actor model | 6 |
| 0x70-0x7F | Pattern matching | 7 |
| 0x80-0x8F | Process control | 3 |
| 0x90-0x9F | Type casting | 3 |
| 0xA0-0xAF | Control flow | 7 |

---

## Compiler.hs - AST to Bytecode

### Compilation Strategies

#### 1. Expression Compilation

**Strategy**: Compile to stack operations

```haskell
compileExpr :: Expr -> Bytecode
```

##### Literals

**Source**:
```ratatouille
42
"hello"
:ok
```

**Bytecode**:
```
PUSH_INT 42
PUSH_STRING "hello"
PUSH_ATOM "ok"
```

##### Variables

**Source**:
```ratatouille
x
state
```

**Bytecode**:
```
LOAD_LOCAL "x"      // Local variable
GET_STATE           // Special: process state
```

##### Binary Operations

**Source**:
```ratatouille
a + b
```

**Bytecode**:
```
LOAD_LOCAL "a"
LOAD_LOCAL "b"
ADD
```

**Strategy**: Left operand, right operand, then operator

##### Tuples and Arrays

**Tuples**:
```ratatouille
(1, 2, 3)
```

**Bytecode**:
```
PUSH_INT 1
PUSH_INT 2
PUSH_INT 3
PUSH_TUPLE 3
```

**Arrays**:
```ratatouille
[1, 2, 3]
```

**Bytecode**:
```
PUSH_INT 1
PUSH_INT 2
PUSH_INT 3
PUSH_ARRAY 3
```

##### Conditionals

**Source**:
```ratatouille
if x > 0 then :pos else :neg
```

**Bytecode**:
```
LOAD_LOCAL "x"
PUSH_INT 0
CMP_GT
JUMP_IF_FALSE 3        // Skip then-branch
PUSH_ATOM "pos"
JUMP 2                 // Skip else-branch
PUSH_ATOM "neg"
```

**Pattern**: Condition → jump if false → then-branch → jump over else → else-branch

##### Function Calls

**Source**:
```ratatouille
add(2, 3)
```

**Bytecode**:
```
PUSH_INT 2
PUSH_INT 3
CALL_FUNCTION "add" 2
```

##### Message Sending

**Source**:
```ratatouille
pid <- :increment
```

**Bytecode**:
```
LOAD_LOCAL "pid"
PUSH_ATOM "increment"
SEND
```

##### Receive Blocks

**Source**:
```ratatouille
receive {
  | :increment -> state + 1
  | :get -> state
}
```

**Bytecode**:
```
WAIT_MESSAGE           // Block until message arrives
DUP                    // Preserve message for next pattern
MATCH_ATOM "increment" 5   // Jump 5 if no match
  GET_STATE
  PUSH_INT 1
  ADD
  JUMP 2               // Skip to end
DUP
MATCH_ATOM "get" 2
  GET_STATE
```

**Pattern**: WAIT_MESSAGE → DUP → MATCH → action → JUMP to end

#### 2. Statement Compilation

**Strategy**: Side effects with optional stack manipulation

```haskell
compileStmt :: Stmt -> Bytecode
```

##### Let Bindings

**Source**:
```ratatouille
let x = 42
```

**Bytecode**:
```
PUSH_INT 42
STORE_LOCAL "x"
```

##### Destructuring

**Source**:
```ratatouille
let (x, y) = (10, 20)
```

**Bytecode**:
```
PUSH_INT 10
PUSH_INT 20
PUSH_TUPLE 2
MATCH_TUPLE 2 0        // Expect 2 elements
MATCH_VAR "y"          // Pop and bind y
MATCH_VAR "x"          // Pop and bind x
```

##### Assignment

**Source**:
```ratatouille
x = 42
```

**Bytecode**:
```
PUSH_INT 42
STORE_LOCAL "x"
```

**Special Case (State)**:
```ratatouille
state = state + 1
```

**Bytecode**:
```
GET_STATE
PUSH_INT 1
ADD
SET_STATE
```

#### 3. Definition Compilation

##### Process Definitions

**Source**:
```ratatouille
proc Counter(initial) {
  state: initial,
  receive {
    | :increment -> state = state + 1
    | :get -> state
  }
}
```

**Bytecode**:
```
DEFINE_PROCESS "Counter" ["initial"] [
  // Parameter binding
  STORE_LOCAL "initial"

  // State initialization
  LOAD_LOCAL "initial"
  INIT_STATE

  // Receive loop
  PROCESS_LOOP
  WAIT_MESSAGE
  // ... pattern matching bytecode ...
]
```

##### Function Definitions

**Source**:
```ratatouille
fn add(a<i32>, b<i32>) -> i32 {
  a + b
}
```

**Bytecode**:
```
DEFINE_FUNCTION "add" ["a", "b"] [
  // Parameter binding (reverse order from stack)
  STORE_LOCAL "b"
  STORE_LOCAL "a"

  // Function body
  LOAD_LOCAL "a"
  LOAD_LOCAL "b"
  ADD

  // Return
  RETURN
]
```

#### 4. Pattern Compilation

**Strategy**: Sequential matching with fallthrough

```haskell
compilePattern :: Pattern -> Bytecode
```

| Pattern | Bytecode | Description |
|---------|----------|-------------|
| `x` | `MATCH_VAR "x"` | Bind to variable |
| `_` | `MATCH_WILDCARD` | Match anything |
| `42` | `MATCH_INT 42 offset` | Match integer |
| `:ok` | `MATCH_ATOM "ok" offset` | Match atom |
| `(x, y)` | `MATCH_TUPLE 2 offset, MATCH_VAR "y", MATCH_VAR "x"` | Match tuple |
| `[x, y]` | `MATCH_ARRAY 2 offset, MATCH_VAR "y", MATCH_VAR "x"` | Match array |

### Compilation Example: Full Program

**Source**:
```ratatouille
proc Counter(init) {
  state: init,
  receive {
    | :inc -> state = state + 1
    | :get -> state
  }
}

proc main() {
  let c = spawn Counter(0)
  c <- :inc
  c <- :inc
  c <- :get
}
```

**Bytecode**:
```
// Define Counter process
DEFINE_PROCESS "Counter" ["init"] [
  STORE_LOCAL "init"
  LOAD_LOCAL "init"
  INIT_STATE
  PROCESS_LOOP
  WAIT_MESSAGE
  DUP
  MATCH_ATOM "inc" 5
    GET_STATE
    PUSH_INT 1
    ADD
    SET_STATE
    JUMP 2
  DUP
  MATCH_ATOM "get" 2
    GET_STATE
]

// Define main process
DEFINE_PROCESS "main" [] [
  // let c = spawn Counter(0)
  PUSH_INT 0
  CREATE_INSTANCE "Counter" 1
  STORE_LOCAL "c"

  // c <- :inc
  LOAD_LOCAL "c"
  PUSH_ATOM "inc"
  SEND

  // c <- :inc
  LOAD_LOCAL "c"
  PUSH_ATOM "inc"
  SEND

  // c <- :get
  LOAD_LOCAL "c"
  PUSH_ATOM "get"
  SEND
]

// Call main
CALL_FUNCTION "main" 0
HALT
```

---

## Encoder.hs - Binary Serialization

### Binary Format

```
┌─────────────────────────────────────┐
│         Header (10 bytes)           │
├─────────────────────────────────────┤
│  Magic Number (4 bytes): "RTBC"    │
│  Version Major (1 byte): 1          │
│  Version Minor (1 byte): 0          │
│  Instruction Count (4 bytes): N     │
├─────────────────────────────────────┤
│    Instruction Stream (variable)    │
│  ┌─────────────────────────────┐   │
│  │ Opcode (1 byte)             │   │
│  │ Operands (variable length)  │   │
│  │ ...                         │   │
│  └─────────────────────────────┘   │
└─────────────────────────────────────┘
```

### Encoding Strategy

**Library**: `Data.Binary.Put` (lazy ByteString)

**Rationale**:
- Efficient for large bytecode files
- Streaming serialization
- Platform-independent

### Encoding Rules

#### Header

```haskell
-- Magic number: "RTBC"
magicNumber = [0x52, 0x54, 0x42, 0x43]

-- Version
versionMajor = 1
versionMinor = 0

-- Instruction count (32-bit little-endian)
instrCount :: Word32
```

#### Instructions

**Format**: `opcode (1 byte) + operands (variable)`

**Examples**:

| Instruction | Encoding |
|-------------|----------|
| `PUSH_INT 42` | `0x01 0x00 <varint 42>` |
| `PUSH_STRING "hi"` | `0x01 0x02 <len 2> "hi"` |
| `ADD` | `0x30` |
| `LOAD_VAR "x"` | `0x10 <len 1> "x"` |
| `JUMP 5` | `0xA0 <varint 5>` |

#### Variable-Length Encoding

**Integers**: ZigZag encoding for signed integers

**Strings**:
```
length (4 bytes, little-endian) + UTF-8 bytes
```

**Lists**:
```
count (4 bytes) + elements
```

### Encoding Functions

```haskell
encodeBytecode :: Bytecode -> ByteString
encodeInstruction :: Instruction -> Put
encodeValue :: Value -> Put
encodeText :: Text -> Put
encodeInteger :: Integer -> Put
```

---

## Decoder.hs - Binary Deserialization

### Decoding Strategy

**Library**: `Data.Binary.Get`

**Error Handling**: `Either String Bytecode`

### Decoding Process

1. **Read Header**: Validate magic number and version
2. **Read Instruction Count**: Determine how many instructions to decode
3. **Decode Instructions**: Read each instruction sequentially
4. **Validation**: Ensure bytecode is well-formed

### Validation Checks

```haskell
-- 1. Magic number must match "RTBC"
if magic /= "RTBC" then fail "Invalid magic number"

-- 2. Version must be supported
if version /= (1, 0) then fail "Unsupported version"

-- 3. Instruction count must match actual count
if length instructions /= instrCount then fail "Instruction count mismatch"

-- 4. All instructions must be valid
-- (checked during decoding)
```

### Decoding Functions

```haskell
decodeBytecode :: ByteString -> Either String Bytecode
decodeInstruction :: Get Instruction
decodeValue :: Get Value
decodeText :: Get Text
decodeInteger :: Get Integer
```

### Error Messages

**Examples**:
```
"Invalid magic number - not a Ratatouille bytecode file"
"Unsupported version: 2.0"
"Unknown opcode: 0xFF"
"Unexpected end of input while reading instruction"
```

---

## Stack Machine Model

### Execution Model

**Principle**: All operations manipulate an implicit stack

**Example**:
```
Expression: 2 + 3 * 4

Bytecode:
  PUSH_INT 2       Stack: [2]
  PUSH_INT 3       Stack: [3, 2]
  PUSH_INT 4       Stack: [4, 3, 2]
  MUL              Stack: [12, 2]  (pop 4 and 3, push 12)
  ADD              Stack: [14]     (pop 12 and 2, push 14)
```

### Stack Conventions

**Function Calls**:
1. Push arguments left-to-right
2. Call function (args consumed, result pushed)

**Example**:
```ratatouille
add(2, 3)
```

**Execution**:
```
PUSH_INT 2           Stack: [2]
PUSH_INT 3           Stack: [3, 2]
CALL_FUNCTION "add"  Stack: [5]  (args consumed, result pushed)
```

### Stack Growth

**Direction**: Grows downward (head of list = top of stack)

```haskell
type Stack = [Value]  -- Head is top

push :: Value -> Stack -> Stack
push v stack = v : stack

pop :: Stack -> (Value, Stack)
pop (v:rest) = (v, rest)
```

---

## Performance Characteristics

### Compilation

| Operation | Time Complexity | Space Complexity |
|-----------|----------------|------------------|
| Compile Literal | O(1) | O(1) |
| Compile Binary Op | O(left + right) | O(instructions) |
| Compile If-Else | O(cond + then + else) | O(instructions) |
| Compile Receive | O(patterns × actions) | O(instructions) |
| Full Program | O(AST size) | O(bytecode size) |

### Encoding/Decoding

| Operation | Time Complexity | Space Complexity |
|-----------|----------------|------------------|
| Encode Instruction | O(operands) | O(1) |
| Encode Bytecode | O(instructions) | O(bytecode size) |
| Decode Instruction | O(operands) | O(1) |
| Decode Bytecode | O(instructions) | O(bytecode size) |

**Note**: Lazy ByteString provides streaming, so actual memory usage is lower

---

## Optimization Opportunities

### Planned Optimizations

#### 1. Peephole Optimization

**Pattern**: Optimize instruction sequences

```
// Before
PUSH_INT 2
PUSH_INT 3
ADD

// After
PUSH_INT 5
```

#### 2. Constant Folding

**Pattern**: Evaluate constants at compile time

```ratatouille
let x = 2 + 3  // Compile as: PUSH_INT 5, STORE_VAR "x"
```

#### 3. Dead Code Elimination

**Pattern**: Remove unreachable code

```ratatouille
if true then x else y  // Compile only: x
```

#### 4. Tail Call Optimization

**Pattern**: Convert tail calls to jumps

```
// Before
CALL_FUNCTION "factorial"
RETURN

// After
JUMP "factorial"
```

---

## Best Practices

### 1. Instruction Selection

**Prefer specialized instructions over general ones**:

```
// Good: Direct increment
INC_VAR "counter"

// Less efficient
LOAD_LOCAL "counter"
PUSH_INT 1
ADD
STORE_LOCAL "counter"
```

### 2. Register Allocation (Future)

**Strategy**: Reuse stack slots for temporary values

### 3. Jump Offset Calculation

**Ensure correct offsets**:
- Count instructions carefully
- Account for multi-byte instructions
- Test with nested control flow

---

## Testing Strategy

### Unit Tests

**Test individual instruction encoding/decoding**:

```haskell
testPushInt = do
  let instr = PUSH_INT 42
  let encoded = encodeInstruction instr
  let decoded = decodeInstruction encoded
  assertEqual instr decoded
```

### Integration Tests

**Test compilation of full programs**:

```haskell
testCounterProgram = do
  let source = "proc Counter(init) { ... }"
  let ast = parse source
  let bytecode = compileProgram ast
  let encoded = encodeBytecode bytecode
  let decoded = decodeBytecode encoded
  assertEqual bytecode decoded
```

### Property Tests

**Use QuickCheck for roundtrip testing**:

```haskell
prop_encodeDecodeRoundtrip :: Bytecode -> Bool
prop_encodeDecodeRoundtrip bc =
  (decodeBytecode . encodeBytecode) bc == Right bc
```

---

## Related Documentation

- [AST Module](AST_MODULE.md) - Abstract Syntax Tree structure
- [VM Module](VM_MODULE.md) - Bytecode execution
- [Architecture Overview](ARCHITECTURE.md) - System architecture
- [Parser Module](PARSER_MODULE.md) - Source → AST

---

**Document Version**: 1.0
**Maintainers**: GLaDOS Development Team
