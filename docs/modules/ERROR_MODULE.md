# Error Handling Module Documentation

**Project**: GLaDOS/Ratatouille
**Version**: 3.0.0
**Last Updated**: November 2025

---

## Overview

The Error module provides comprehensive error handling and reporting for Ratatouille, with rich context information and user-friendly formatting. It transforms raw parse errors into helpful messages with source location, context hierarchy, and color-coded output.

### Location

```
src/Ratatouille/Error/
  ├── Types.hs     - Error type definitions
  ├── Context.hs   - Context extraction from source code
  └── Format.hs    - ANSI color formatting for terminal output
```

### Key Features

- **Contextual Errors**: Show where in the code structure the error occurred
- **Source Location**: Line/column information with surrounding code
- **Color-Coded Output**: ANSI colors for readability
- **Context Hierarchy**: Procedure → block → statement context
- **Helpful Suggestions**: Expected vs. actual tokens

---

## Architecture

### Error Flow

```
Parse Error → Extract Context → Format → Display
     ↓              ↓              ↓         ↓
 Raw Megaparsec   Source       ANSI      Terminal
   Message        Analysis     Colors     Output
```

---

## Types.hs - Error Definitions

### RichError

```haskell
data RichError = RichError
  { reFile :: FilePath      -- Source file path
  , reRaw  :: String        -- Raw error message from parser
  , reLine :: Int           -- Error line number
  , reContexts :: [String]  -- Context hierarchy
  }
```

**Purpose**: Enriched error with contextual information

**Example**:
```haskell
RichError
  { reFile = "example.rat"
  , reRaw = "unexpected '}', expecting expression"
  , reLine = 15
  , reContexts = ["in procedure 'Counter'", "inside a receive block", "in case pattern"]
  }
```

### makeRichError

```haskell
makeRichError :: FilePath → String → Text → RichError
makeRichError fp raw content =
  let line = getErrorLine raw
      contexts = extractErrorContext content line
  in RichError fp raw line contexts
```

**Process**:
1. Parse raw error to extract line number
2. Analyze source code for context
3. Build context hierarchy
4. Return enriched error

---

## Context.hs - Context Extraction

### extractErrorContext

```haskell
extractErrorContext :: Text → Int → [String]
```

**Purpose**: Build hierarchy of contexts where error occurred

**Example**:
```ratatouille
proc Counter(init) {
  state: init,
  receive {
    | :inc -> state = state +   // <-- ERROR HERE
  }
}
```

**Context Hierarchy**:
```
["in procedure 'Counter'", "inside a receive block", "in case body"]
```

### Context Types

#### 1. Procedure Context

```haskell
findMostRecentProc :: [String] → Maybe String
```

**Detects**:
```ratatouille
proc Counter(init) { ... }  // → "in procedure 'Counter'"
```

**Implementation**: Search backward for `proc` keyword, extract name

#### 2. Block Context

```haskell
findBlockContexts :: [String] → [String]
```

**Detects**:
- `receive { ... }` → "inside a receive block"
- `match expr { ... }` → "inside a match expression"
- `if cond then { ... }` → "inside an if expression"
- `loop { ... }` → "inside a loop"

**Implementation**: Track unclosed braces for keywords

#### 3. Statement Context

```haskell
findStatementContext :: [String] → String → Maybe String
```

**Detects**:
- `state: ...` → "in state initialization"
- `let x = ...` → "in let binding"
- `pid <- ...` → "in send statement"
- `spawn Proc()` → "in spawn expression"
- `| pattern ->` → "in case pattern" or "in case body"

---

## Format.hs - Error Formatting

### ANSI Color Codes

```haskell
reset   = "\ESC[0m"
bold    = "\ESC[1m"
red     = "\ESC[31m"
yellow  = "\ESC[33m"
blue    = "\ESC[34m"
cyan    = "\ESC[36m"
magenta = "\ESC[35m"
dim     = "\ESC[2m"
```

### formatRichError

```haskell
formatRichError :: RichError → String
```

**Output Format**:
```
Parse error in example.rat:15
  - in procedure 'Counter'
  - inside a receive block
  - in case pattern

unexpected '}', expecting expression
```

**Color Scheme**:
- **Header**: Bold red
- **Context bullets**: Bold cyan
- **Error message**: Default formatting

### formatError

```haskell
formatError :: String → String
```

**Purpose**: Color-code Megaparsec error output

**Input** (raw Megaparsec):
```
example.rat:15:20:
  |
15|     | :inc -> state = state +
  |                              ^
unexpected end of input
expecting expression
```

**Output** (color-coded):
```ansi
 at example.rat:15:20:
  |
15|     | :inc -> state = state +
  |                              ^
UNEXPECTED: end of input
EXPECTED: expression
```

**Colors**:
- File path: Bold blue
- Line numbers: Bold magenta
- Source code: White
- Unexpected: Bold red
- Expected: Bold green/yellow

---

## Error Examples

### Example 1: Missing Semicolon

**Source**:
```ratatouille
proc main() {
  let x = 10
  let y = 20  // Missing in Ratatouille (no semicolons needed, actually!)
  print(x)
}
```

**Error Output**:
```ansi
Parse error in example.rat:3
  - in procedure 'main'
  - in let binding

unexpected 'let', expecting '}' or statement
```

### Example 2: Unclosed Brace

**Source**:
```ratatouille
proc Counter(init) {
  state: init,
  receive {
    | :inc -> state = state + 1
  // Missing closing brace
}
```

**Error Output**:
```ansi
Parse error in example.rat:6
  - in procedure 'Counter'
  - inside a receive block

unexpected end of input, expecting '}'
```

### Example 3: Type Mismatch

**Source**:
```ratatouille
let x<i32> = "not an integer"
```

**Error Output**:
```ansi
Parse error in example.rat:1
  - in let binding
  - in type annotation

Type mismatch: expected i32, got string
```

### Example 4: Pattern Match Error

**Source**:
```ratatouille
receive {
  | :inc x -> state + x  // Should be (:inc, x)
}
```

**Error Output**:
```ansi
Parse error in example.rat:2
  - inside a receive block
  - in case pattern

unexpected 'x', expecting '->' or pattern continuation
```

---

## Error Categories

### 1. Parse Errors (Compile-Time)

**When**: During parsing (source → AST)

**Examples**:
- Syntax errors
- Unexpected tokens
- Missing delimiters
- Invalid keywords

**Handling**: Report and abort compilation

### 2. Semantic Errors (Compile-Time)

**When**: During AST validation

**Examples**:
- Missing main() procedure
- Undefined variables (some checked at runtime)
- Type annotation mismatches

**Handling**: Report and abort compilation

### 3. Runtime Errors (Execution-Time)

**When**: During VM execution

**Examples**:
- Division by zero
- Stack underflow
- Undefined variable access
- Pattern match failure
- Process not found

**Handling**: Crash process (not entire VM)

### 4. User Errors (Application Logic)

**When**: Application-level logic

**Examples**:
- File not found
- Invalid input
- Network errors

**Handling**: Explicit Either type, pattern matching

**Example**:
```ratatouille
let result<i32!string> = parseNumber(input)
match result {
  | ok(value) -> value + 1
  | ko(err) -> print(err)
}
```

---

## Best Practices

### 1. Provide Context

Always include:
- Where (file, line, column)
- What (error type)
- Why (expected vs. actual)
- How to fix (suggestion)

### 2. Use Colors Wisely

- **Red**: Errors, problems
- **Yellow**: Warnings, caution
- **Green**: Success, expected values
- **Blue/Cyan**: Context, metadata
- **Magenta**: Line numbers

### 3. Show Code Context

Include:
- Current line
- Previous 1-2 lines
- Next 1-2 lines
- Highlighting (^)

**Example**:
```
13| proc Counter(init) {
14|   state: init,
15|   receive {
    |            ^
unexpected end of line
```

### 4. Suggest Fixes

**Examples**:
```
unexpected '}', expecting expression

  Suggestion: Did you forget to provide a value?
```

```
undefined variable 'x'

  Suggestion: Did you mean 'counter'? (similar names)
```

---

## Integration with Parser

### Megaparsec Error Handling

```haskell
case parse pProgram filepath content of
  Left err → do
    let rawMsg = errorBundlePretty err
    let richError = makeRichError filepath rawMsg content
    putStrLn $ formatRichError richError
    exitFailure
  Right ast → compileProgram ast
```

**Flow**:
1. Megaparsec returns `ParseErrorBundle`
2. Convert to pretty string with `errorBundlePretty`
3. Enrich with context via `makeRichError`
4. Format with colors via `formatRichError`
5. Display to user

---

## Testing Strategy

### Unit Tests

**Test context extraction**:
```haskell
testProcContext = do
  let source = "proc Counter(init) {\n  error here\n}"
  let contexts = extractErrorContext source 2
  assertEqual ["in procedure 'Counter'"] contexts
```

### Integration Tests

**Test full error reporting**:
```haskell
testErrorReporting = do
  let source = "proc main() { let x = }"
  case parse pProgram "test.rat" source of
    Left err → do
      let richError = makeRichError "test.rat" (errorBundlePretty err) source
      assertContains (formatRichError richError) "in procedure 'main'"
```

---

## Future Enhancements

### Planned Features

1. **Error Recovery**: Continue parsing after errors to find multiple errors
2. **Suggestions**: Fuzzy matching for typos (e.g., "Did you mean 'counter'?")
3. **Quick Fixes**: Automated fix suggestions
4. **Error Codes**: Unique identifiers for error types (E0001, E0002)
5. **JSON Output**: Machine-readable error format for IDEs
6. **Stack Traces**: Full stack trace for runtime errors

### Error Codes (Future)

| Code | Category | Example |
|------|----------|---------|
| E0001 | Syntax | Unexpected token |
| E0002 | Semantic | Undefined variable |
| E0003 | Type | Type mismatch |
| E0004 | Runtime | Division by zero |
| E0005 | Process | Process not found |

---

## Related Documentation

- [Parser Module](PARSER_MODULE.md) - Error generation
- [AST Module](AST_MODULE.md) - Semantic validation
- [VM Module](VM_MODULE.md) - Runtime errors
- [Architecture Overview](ARCHITECTURE.md) - System design

---

**Document Version**: 1.0
**Maintainers**: GLaDOS Development Team
