# Parser Module Documentation

**Project**: GLaDOS/Ratatouille
**Version**: 3.0.0
**Last Updated**: November 2025

---

## Overview

The Parser module is responsible for transforming Ratatouille source code (`.rat` files) into an Abstract Syntax Tree (AST). It uses **Megaparsec**, a powerful parser combinator library that provides excellent error messages and composable parsing logic.

### Location

```
src/Ratatouille/Parser/
  ├── Common.hs       - Lexical analysis and common parsers
  ├── ExprStmt.hs     - Expression and statement parsing
  ├── Proc.hs         - Process and function definition parsing
  └── Pattern.hs      - Pattern matching syntax parsing
```

### Key Features

- **Parser Combinators**: Small, composable parsing functions
- **Operator Precedence**: 9 precedence levels for correct expression parsing
- **Error Recovery**: Context-aware error messages with source location
- **Comment Support**: Line (`//`) and block (`/* */`) comments
- **Type Annotations**: Optional type annotations on variables

---

## Architecture

### Parser Type

```haskell
type Parser = Parsec Void Text
```

All parsers use Megaparsec's `Parsec` monad transformer with:
- **Error Type**: `Void` (no custom errors, using defaults)
- **Stream Type**: `Text` (UTF-8 text, more efficient than String)

### Parsing Pipeline

```
Source Text → Lexical Analysis → Token Stream → Syntax Analysis → AST
```

---

## Common.hs - Lexical Foundation

### Purpose

Provides fundamental lexical analysis and basic token parsers used across all parser modules.

### Key Components

#### 1. Whitespace and Comments

```haskell
sc :: Parser ()
```

**Purpose**: Skip whitespace and comments
**Handles**:
- Spaces, tabs, newlines
- Line comments: `// comment`
- Block comments: `/* comment */`

**Usage**: Automatically applied by `lexeme` and `symbol`

#### 2. Lexeme and Symbol Parsers

```haskell
lexeme :: Parser a -> Parser a
symbol :: Text -> Parser Text
```

**Purpose**: Parse tokens with automatic trailing whitespace consumption

**Example**:
```haskell
-- Parse "let" keyword followed by whitespace
pLet = symbol "let"

-- Parse identifier with trailing whitespace
pIdentifier = lexeme $ takeWhile1P (Just "identifier") isAlphaNum
```

#### 3. Identifier Parser

```haskell
pIdentifier :: Parser Text
```

**Rules**:
- Must start with letter or underscore: `[a-zA-Z_]`
- Can contain letters, digits, underscores: `[a-zA-Z0-9_]*`
- Cannot be a reserved word

**Reserved Words**:
```haskell
proc, receive, spawn, let, const, if, then, else,
self, none, void, true, false, scast, rcast, ccast,
just, ok, ko, import, from, match
```

**Example Valid Identifiers**:
- `counter`
- `_private`
- `value42`
- `calculateSum`

**Invalid Identifiers**:
- `42value` (starts with digit)
- `let` (reserved word)
- `my-var` (contains hyphen)

#### 4. Literal Parsers

##### Integer Literals

```haskell
pIntLiteral :: Parser Literal
```

**Supports**:
- Untyped integers: `42`, `-17`, `1000`
- Typed integers: `42i8`, `100u64`, `-5i32`

**Numeric Types**:
- Signed: `i8`, `i16`, `i32`, `i64`
- Unsigned: `u8`, `u16`, `u32`, `u64`

**Examples**:
```ratatouille
let x = 42           // LInt 42
let y = 100u8        // LTypedInt U8 100
let z = -5i32        // LTypedInt I32 (-5)
```

##### Float Literals

```haskell
pFloatLiteral :: Parser Literal
```

**Supports**:
- Untyped floats: `3.14`, `-2.5`, `0.0`
- Typed floats: `3.14f32`, `2.5f64`

**Examples**:
```ratatouille
let pi = 3.14159     // LFloat 3.14159
let x = 2.0f32       // LTypedFloat F32 2.0
```

##### String Literals

```haskell
pStringLiteral :: Parser Literal
```

**Syntax**: Double-quoted strings with escape sequences

**Examples**:
```ratatouille
"Hello, world!"
"Line 1\nLine 2"
"Tab\there"
""  // Empty string
```

##### Boolean and None Literals

```haskell
pBoolLiteral :: Parser Literal
pNoneLiteral :: Parser Literal
```

**Examples**:
```ratatouille
true
false
none
```

#### 5. Atom Parser

```haskell
pAtom :: Parser Expr
```

**Syntax**: Colon followed by identifier

**Purpose**: Symbolic constants for message tags

**Examples**:
```ratatouille
:ok
:error
:increment
:ping
```

**Use Cases**:
```ratatouille
receive {
  | :increment -> state = state + 1
  | :reset -> state = 0
}

pid <- :ping
```

#### 6. Type Annotation Parsers

##### Basic Types

```haskell
pType :: Parser Type
```

**Supported Types**:
- **Numeric**: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`
- **String**: `string`
- **Boolean**: `bool`
- **Process ID**: `pid`
- **Atom**: `atom`
- **None/Null**: `none`
- **Any**: `any` or `auto`

**Examples**:
```ratatouille
let x<i32> = 42
let name<string> = "Alice"
let active<bool> = true
let process<pid> = spawn Counter(0)
```

##### Compound Types

**Tuples**:
```ratatouille
let point<(i32, i32)> = (10, 20)
let triple<(string, i32, bool)> = ("test", 42, true)
```

**Arrays**:
```ratatouille
let numbers<[i32]> = [1, 2, 3]      // Dynamic array
let fixed<[i32, 10]> = [0; 10]      // Fixed-size array
```

**Maybe (Optional) Types**:
```ratatouille
let maybeValue<i32?> = just(42)
let nothing<string?> = none
```

**Either (Result) Types**:
```ratatouille
let result<i32!string> = ok(42)      // VRight (VInt 42)
let error<i32!string> = ko("Failed") // VLeft (VString "Failed")
```

##### Return Types

```haskell
pReturnType :: Parser Type
```

**Special**: Allows `void` for functions with no return value

**Examples**:
```ratatouille
proc main() -> void {
  // No return value
}

fn add(a<i32>, b<i32>) -> i32 {
  a + b
}
```

---

## ExprStmt.hs - Expression and Statement Parsing

### Purpose

Parses expressions (computable values) and statements (side-effecting operations).

### Operator Precedence

**Implementation**: Precedence climbing with 9 levels

**Precedence Table** (lowest to highest):

| Level | Operators | Associativity | Example |
|-------|-----------|---------------|---------|
| 1 | `=` | Right | `x = y = 5` |
| 2 | `<-` | Right | `pid <- msg` |
| 3 | `\|\|` | Left | `a \|\| b \|\| c` |
| 4 | `&&` | Left | `a && b && c` |
| 5 | `==`, `!=`, `<`, `>`, `<=`, `>=` | Left | `x == y` |
| 6 | `+`, `-`, `++` | Left | `a + b - c` |
| 7 | `*`, `/` | Left | `a * b / c` |
| 7.5 | `!`, `-`, `+` (unary) | Right | `!flag`, `-x` |
| 8 | `.`, `[]` | Left | `obj.field[0]` |
| 9 | Base expressions | N/A | Literals, calls |

**Example**:
```ratatouille
let result = a + b * c && d == e  // Parsed correctly as: (a + (b * c)) && (d == e)
```

### Expression Types

#### 1. Literals

```ratatouille
42                  // Integer
3.14                // Float
"text"              // String
true, false         // Boolean
none                // None
:atom               // Atom
```

#### 2. Variables

```ratatouille
x                   // Variable reference
self                // Current process PID
```

#### 3. Binary Operations

**Arithmetic**:
```ratatouille
a + b               // Addition
a - b               // Subtraction
a * b               // Multiplication
a / b               // Division
```

**Comparison**:
```ratatouille
a == b              // Equality
a != b              // Inequality
a < b               // Less than
a > b               // Greater than
a <= b              // Less than or equal
a >= b              // Greater than or equal
```

**Logical**:
```ratatouille
a && b              // Logical AND
a || b              // Logical OR
```

**String Concatenation**:
```ratatouille
"Hello, " ++ "world!"  // String concatenation
```

#### 4. Unary Operations

```ratatouille
!flag               // Logical negation
-x                  // Arithmetic negation
+x                  // Unary plus
```

#### 5. Increment/Decrement

```ratatouille
++x                 // Pre-increment (increment then return)
x++                 // Post-increment (return then increment)
--x                 // Pre-decrement
x--                 // Post-decrement
```

#### 6. Tuples and Arrays

**Tuples**:
```ratatouille
(1, 2, 3)
(x, "text", true)
()                  // Empty tuple (unit)
```

**Arrays**:
```ratatouille
[1, 2, 3, 4, 5]
[]                  // Empty array
[x, y, z]
```

**Array Indexing**:
```ratatouille
arr[0]              // First element
matrix[i][j]        // Multi-dimensional
```

#### 7. Function and Process Calls

```ratatouille
add(2, 3)           // Function call
spawn Counter(0)    // Spawn new process
```

#### 8. Message Sending

```ratatouille
pid <- :increment
pid <- (:add, 5)
```

#### 9. Conditionals

```ratatouille
if x > 0 then {
  positive
} else {
  negative
}

// One-liner
if flag then :yes else :no
```

#### 10. Pattern Matching

**Receive Expression**:
```ratatouille
receive {
  | :ping -> :pong
  | (:add, x) -> state + x
  | _ -> none
}
```

**Match Expression**:
```ratatouille
match value {
  | 0 -> :zero
  | 1 -> :one
  | x -> :other
}
```

#### 11. Type Casting

```ratatouille
scast<i64>(42)      // Static cast (safe)
rcast<u32>(-1)      // Reinterpret cast (unsafe)
ccast(constValue)   // Const cast (remove const)
```

#### 12. Maybe and Either

**Maybe**:
```ratatouille
just(42)            // Some value
none                // No value

value >>= fn        // Monadic bind
```

**Either**:
```ratatouille
ok(42)              // Success (Right)
ko("error")         // Failure (Left)

result >>= handler  // Monadic bind
```

### Statement Types

#### 1. Let Binding

```ratatouille
let x = 10
let y<i32> = 42
let name<string> = "Alice"
```

#### 2. Const Binding

```ratatouille
const PI = 3.14159
const MAX<i32> = 100
```

#### 3. Destructuring Let

```ratatouille
let (x, y) = (10, 20)
let [first, second, rest...] = [1, 2, 3, 4, 5]
```

#### 4. Assignment

```ratatouille
x = 42
state = state + 1
```

#### 5. Expression Statement

```ratatouille
print(x)
pid <- :message
spawn Worker()
```

---

## Proc.hs - Process and Function Definitions

### Purpose

Parses top-level definitions: processes, functions, and imports.

### Process Definitions

**Syntax**:
```ratatouille
proc Name(param1, param2, ...) {
  state: <initial_value>,
  receive {
    | pattern1 -> expression1
    | pattern2 -> expression2
    ...
  }
}
```

**Example**:
```ratatouille
proc Counter(initial) {
  state: initial,
  receive {
    | :increment -> state = state + 1
    | :decrement -> state = state - 1
    | :get -> state
    | :reset -> state = 0
  }
}
```

**Components**:
- **Name**: Process identifier
- **Parameters**: Initialization arguments
- **State**: Optional mutable state
- **Receive**: Message handling patterns

### Function Definitions

**Syntax**:
```ratatouille
fn name(param1<Type1>, param2<Type2>, ...) -> ReturnType {
  expression
}
```

**Example**:
```ratatouille
fn add(a<i32>, b<i32>) -> i32 {
  a + b
}

fn factorial(n<i32>) -> i32 {
  if n <= 1 then 1 else n * factorial(n - 1)
}
```

**Restrictions**:
- Pure functions only (no side effects)
- Cannot spawn, send, or receive messages
- Must return a value (or void)

### Import Declarations

**Syntax**:
```ratatouille
// Import all exports
import "path/to/module.rat"

// Import specific items
import {Counter, Worker} from "utils.rat"

// Import single item
import Counter from "counter.rat"
```

**Examples**:
```ratatouille
import "stdlib/math.rat"
import {add, multiply} from "../utils/math.rat"
import Counter from "./actors/counter.rat"
```

---

## Pattern.hs - Pattern Matching

### Purpose

Parses patterns used in `receive` blocks and `match` expressions for destructuring and message routing.

### Pattern Types

#### 1. Variable Pattern

**Syntax**: `identifier`

**Purpose**: Bind matched value to variable

**Example**:
```ratatouille
receive {
  | x -> x + 1  // x binds to the message
}
```

#### 2. Typed Variable Pattern

**Syntax**: `identifier<Type>`

**Example**:
```ratatouille
receive {
  | value<i32> -> value * 2
  | const name<string> -> name
}
```

#### 3. Wildcard Pattern

**Syntax**: `_`

**Purpose**: Match anything, discard value

**Example**:
```ratatouille
receive {
  | _ -> none  // Catch-all case
}
```

#### 4. Literal Pattern

**Syntax**: Literal values

**Example**:
```ratatouille
match x {
  | 0 -> :zero
  | 1 -> :one
  | 42 -> :answer
  | "hello" -> :greeting
  | true -> :yes
}
```

#### 5. Atom Pattern

**Syntax**: `:atom`

**Purpose**: Match message tags

**Example**:
```ratatouille
receive {
  | :ping -> :pong
  | :increment -> state + 1
  | :reset -> 0
}
```

#### 6. Tuple Pattern

**Syntax**: `(pattern1, pattern2, ...)`

**Purpose**: Destructure tuples

**Example**:
```ratatouille
receive {
  | (:add, x, y) -> x + y
  | (:point, x, y) -> x * x + y * y
  | (first, _, third) -> first + third
}
```

#### 7. Array Pattern

**Syntax**: `[pattern1, pattern2, ...]`

**Purpose**: Destructure arrays

**Example**:
```ratatouille
let [first, second, rest...] = [1, 2, 3, 4, 5]
// first = 1, second = 2, rest = [3, 4, 5]

match list {
  | [] -> :empty
  | [x] -> :single
  | [x, y] -> :pair
  | [first, rest...] -> :many
}
```

#### 8. Varargs Pattern

**Syntax**: `identifier...`

**Purpose**: Capture remaining elements

**Example**:
```ratatouille
let [head, tail...] = [1, 2, 3, 4]
// head = 1, tail = [2, 3, 4]

receive {
  | (:sum, values...) -> sum(values)
}
```

---

## Error Handling

### Parse Error Reporting

**Features**:
- Source location (line, column)
- Context snippet (surrounding code)
- Expected vs. actual tokens
- Helpful suggestions

**Example Error**:
```
Error parsing file: example.rat
Line 5, column 12:
  let x = ;
          ^
  Expected expression, but found ';'

  Suggestion: Did you forget to provide a value?
```

### Error Recovery

**Strategy**: Megaparsec provides automatic error recovery at statement boundaries

**Benefits**:
- Multiple errors reported at once
- Faster debugging cycle
- Better developer experience

---

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Identifier parsing | O(n) | n = identifier length |
| Expression parsing | O(n) | n = expression size |
| Precedence climbing | O(n) | Single pass |
| Pattern matching | O(m) | m = pattern complexity |

### Space Complexity

- **AST Size**: O(n) where n = source size
- **Parser State**: O(d) where d = nesting depth

---

## Best Practices

### 1. Operator Precedence

Always be aware of operator precedence to avoid unexpected parsing:

```ratatouille
// Incorrect assumption
x = y + z * 2  // Parsed as: x = (y + (z * 2))

// Explicit parentheses for clarity
x = (y + z) * 2
```

### 2. Pattern Exhaustiveness

Always include a catch-all pattern to avoid runtime failures:

```ratatouille
receive {
  | :expected_case -> handle_it
  | _ -> none  // Catch unexpected messages
}
```

### 3. Type Annotations

Use type annotations for clarity and documentation:

```ratatouille
// Good: Clear intent
let count<i32> = 0

// Less clear: Type inferred
let count = 0
```

---

## Future Enhancements

### Planned Features

- [ ] **String Interpolation**: `"Hello, #{name}!"`
- [ ] **List Comprehensions**: `[x * 2 | x <- [1..10]]`
- [ ] **Guard Patterns**: `| x when x > 0 -> positive`
- [ ] **Named Arguments**: `spawn Counter(initial: 0)`
- [ ] **Spread Operator**: `{...defaults, name: "Alice"}`

---

## Related Documentation

- [AST Module](AST_MODULE.md) - Abstract Syntax Tree structure
- [Bytecode Compiler](BYTECODE_MODULE.md) - AST to bytecode transformation
- [Language Syntax](ratatouille.ebnf) - Formal grammar specification
- [Architecture Overview](ARCHITECTURE.md) - System-wide architecture

---

**Document Version**: 1.0
**Maintainers**: GLaDOS Development Team
