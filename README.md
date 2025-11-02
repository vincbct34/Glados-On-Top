# GLaDOS - Ratatouille Programming Language

[![CI](https://github.com/vincbct34/Glados-On-Top/workflows/CI/badge.svg)](https://github.com/vincbct34/Glados-On-Top/actions)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Version](https://img.shields.io/badge/version-3.0.0-green.svg)](CHANGELOG.md)

> **G**eneric **L**anguage **A**nd **D**ata **O**perand **S**yntax

A modern, actor-model programming language with strong type safety, process isolation, and message-passing concurrency. Built in Haskell with educational goals in mind.

---

## 🌟 Features

### Core Language Features
- ✨ **Actor Model Concurrency**: Process-based isolation with message passing
- 🔒 **Memory Safety**: Garbage-collected runtime prevents use-after-free and buffer overflows
- 🎯 **Strong Type System**: 10 numeric types, Maybe/Either, tuples, and arrays
- 🔍 **Pattern Matching**: Powerful message routing and destructuring
- 🚀 **Process Spawning**: Lightweight concurrent processes
- 📬 **Message Passing**: Safe, copy-based communication between processes
- 🛡️ **Explicit Safety**: Clear distinction between safe and unsafe operations
- 🧩 **Functional + Imperative**: Best of both paradigms

### Developer Experience
- 📝 Formal EBNF grammar specification
- 🧪 Comprehensive test suite (4,949 lines of tests!)
- 📚 Extensive documentation
- 🔧 Easy-to-use toolchain (compiler + VM)
- 🎓 Educational codebase with clear architecture

---

## 🚀 Quick Start

### Prerequisites

- **Haskell Stack** (recommended) or **GHC 9.10.2+**
- **Make** (for build automation)
- **Git** (for version control)

### Installation

```bash
# Clone the repository
git clone https://github.com/your-username/Glados-On-Top.git
cd Glados-On-Top

# Build the project
make build

# Run tests
make tests_run

# Run with coverage
make coverage
```

### Hello World

Create a file `hello.rat`:

```ratatouille
proc Greeter() {
  receive {
    | name -> name
  }
}

proc main() {
  let greeter = spawn Greeter()
  greeter <- "World"
}
```

Compile and run:

```bash
# Compile to bytecode
./glados hello.rat -o hello.rtbc

# Execute with VM
./glados-vm hello.rtbc
```

---

## 📖 Language Overview

### Process Definitions

Processes are the core abstraction in Ratatouille:

```ratatouille
/* Pure function process */
proc add(a, b) {
  a + b
}

/* Actor process with state */
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

### Type System

Explicit type annotations prevent errors:

```ratatouille
let count<i32> = 42
let name<string> = "Alice"
let position<(i32, i32)> = (10, 20)
let arr<[f64]> = [1.0, 2.0, 3.0]
```

**10 Numeric Types**: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`

### Maybe and Either Types

No null pointer exceptions:

```ratatouille
let result<i32?> = just(42)
let empty<i32?> = none

match result {
  | just(value) -> value * 2
  | none -> 0
}
```

Explicit error handling:

```ratatouille
let result<string!i32> = ok(42)

match result {
  | ok(value) -> value + 1
  | ko(error) -> 0
}
```

### Pattern Matching

Powerful pattern matching for messages and data:

```ratatouille
receive {
  | (:add, a, b) -> a + b
  | (:mul, a, b) -> a * b
  | (:get, sender) -> sender <- state
  | _ -> none
}
```

### Message Passing

Safe, isolated communication:

```ratatouille
let counter = spawn Counter(0)
counter <- :increment
counter <- :increment
counter <- :get
```

### Type Casting

Three types of casts with explicit safety:

```ratatouille
/* Safe cast (runtime checked) */
let x<i64> = scast<i64>(42)

/* Unsafe cast (bit reinterpretation) */
let bits<u32> = rcast<u32>(-1)  /* UNSAFE */

/* Const cast (remove immutability) */
let mutable = ccast(constValue)  /* UNSAFE */
```

---

## 🏗️ Architecture

### Compilation Pipeline

```
┌─────────────┐
│ Source.rat  │
└──────┬──────┘
       │ Parser (Megaparsec)
       ▼
┌─────────────┐
│     AST     │
└──────┬──────┘
       │ Compiler
       ▼
┌─────────────┐
│  Bytecode   │
└──────┬──────┘
       │ Encoder
       ▼
┌─────────────┐
│ Binary.rtbc │
└──────┬──────┘
       │ VM Decoder
       ▼
┌─────────────┐
│  Execution  │
└─────────────┘
```

### System Components

- **Parser**: Megaparsec-based with operator precedence climbing
- **Compiler**: AST to stack-based bytecode transformation
- **VM**: Stack-based interpreter with process isolation
- **Runtime**: STM-based message passing and process management

For detailed architecture information, see [ARCHITECTURE.md](docs/ARCHITECTURE.md).

---

## 📚 Documentation

### Core Documentation
- [**Grammar Specification**](docs/ratatouille.ebnf) - Formal EBNF grammar
- [**Architecture**](docs/ARCHITECTURE.md) - System design and implementation
- [**Security Analysis**](docs/SECURITY_ANALYSIS.md) - Security features and threat model
- [**Bytecode Architecture**](docs/BYTECODE_ARCHITECTURE.md) - VM instruction set
- [**Language Syntax**](docs/RATATOUILLE_SYNTAX.md) - Syntax reference
- [**Project Strategy**](docs/PROJECT_STRATEGY.md) - Development roadmap

### Developer Resources
- [**CLAUDE.md**](CLAUDE.md) - Development guidelines and workflow
- [**CHANGELOG.md**](CHANGELOG.md) - Version history

---

## 🛠️ Build System

### Makefile Targets

```bash
make build          # Build both compiler and VM
make tests_run      # Run test suite
make coverage       # Generate coverage report
make format         # Format code with Ormolu
make format-check   # Check code formatting
make hlint          # Run HLint analysis
make re             # Full rebuild (clean + build)
make fclean         # Clean all build artifacts
make release-build  # Create release build
```

### Project Structure

```
Glados-On-Top/
├── app/
│   ├── compiler/Main.hs        # glados compiler entry point
│   └── vm/Main.hs              # glados-vm entry point
├── src/Ratatouille/
│   ├── Parser/                 # Parser modules
│   │   ├── Common.hs           # Lexical tokens
│   │   ├── ExprStmt.hs         # Expressions & statements
│   │   ├── Proc.hs             # Process definitions
│   │   └── Pattern.hs          # Pattern matching
│   ├── Bytecode/               # Compiler and bytecode
│   │   ├── Types.hs            # Instruction set
│   │   ├── Compiler.hs         # AST → Bytecode
│   │   ├── Encoder.hs          # Bytecode → Binary
│   │   └── Decoder.hs          # Binary → Bytecode
│   ├── VM/                     # Virtual machine
│   │   ├── VM.hs               # VM state and execution
│   │   ├── Interpreter.hs      # Instruction handlers
│   │   └── Runtime.hs          # Process management
│   ├── Error/                  # Error handling
│   └── AST.hs                  # Abstract Syntax Tree
├── test/                       # Test suite (4,949 lines!)
│   ├── ParserSpec.hs
│   ├── BytecodeSpec.hs
│   ├── VMSpec.hs
│   ├── InterpreterSpec.hs
│   └── ...
├── examples/                   # Example programs (52 files)
│   ├── basics/
│   ├── advanced/
│   └── test/
├── docs/                       # Documentation
└── Makefile                    # Build automation
```

---

## 🧪 Testing

### Comprehensive Test Suite

**Total**: 4,949 lines of tests across 13 files

- **Unit Tests**: Parser, AST, Bytecode, VM, Interpreter, Runtime
- **Integration Tests**: End-to-end compilation and execution
- **Coverage Tests**: HPC-based coverage reporting

### Running Tests

```bash
# Run all tests
make tests_run

# Run specific test file
stack test --test-arguments='--match "Parser"'

# Generate coverage report
make coverage
```

### Example Programs

52 example programs demonstrate language features:

```bash
# Basic examples
examples/basics/helloWorld.rat
examples/basics/counter.rat
examples/basics/Conditionals.rat

# Advanced examples
examples/advanced/Calculator.rat
examples/advanced/Router.rat
examples/advanced/triangularComm.rat
```

---

## 🔒 Security Features

Ratatouille prioritizes security through:

1. **Memory Safety**: Garbage collection eliminates manual memory management bugs
2. **Process Isolation**: Actor model prevents data races and contains failures
3. **Type Safety**: Strong static types catch errors early
4. **Explicit Unsafety**: Dangerous operations clearly marked (`rcast`, `ccast`)
5. **No Null Pointers**: Maybe type eliminates null dereference bugs
6. **Explicit Errors**: Either type forces error handling

**Security Grade**: A- (see [SECURITY_ANALYSIS.md](docs/SECURITY_ANALYSIS.md) for details)

---

## 🎯 Design Philosophy

### Inspirational Languages

Ratatouille combines the best features from:

| Language | Adopted Features |
|----------|------------------|
| **Erlang/Elixir** | Actor model, process isolation, message passing, fault tolerance |
| **Rust** | Explicit unsafe operations, type safety, Option/Result pattern, immutability |
| **Haskell** | Strong type system, algebraic data types, pattern matching, purity |

### Core Principles

1. **Safety over Performance**: Memory safety is paramount
2. **Explicitness over Convenience**: Mark unsafe operations clearly
3. **Isolation over Sharing**: Process boundaries prevent interference
4. **Simplicity over Power**: Easier to learn than Rust/Haskell

---

## 📊 Language Statistics

| Metric | Count |
|--------|-------|
| Source Lines | 4,103 |
| Test Lines | 4,949 |
| Example Programs | 52 |
| Bytecode Instructions | 65+ |
| Numeric Types | 10 |
| Parser Modules | 4 |
| Test Files | 13 |

---

## 🤝 Contributing

### Development Workflow

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests (`make tests_run`)
5. Check formatting (`make format-check`)
6. Run linter (`make hlint`)
7. Commit your changes (`git commit -m 'Add amazing feature'`)
8. Push to the branch (`git push origin feature/amazing-feature`)
9. Open a Pull Request

### Code Quality Standards

- **HLint**: No warnings allowed
- **Ormolu**: Consistent formatting enforced
- **Tests**: Comprehensive coverage required
- **Documentation**: All public APIs documented
- **No Unsafe Functions**: Avoid partial functions and mutable constructs

---

## 🚧 Roadmap

### Version 3.1 (Next Release)
- [ ] Tail call optimization (TCO)
- [ ] Bytecode disassembler
- [ ] File I/O operations
- [ ] Standard library

### Version 4.0 (Future)
- [ ] Module system with imports/exports
- [ ] Generic types (parametric polymorphism)
- [ ] Supervisor trees (fault tolerance)
- [ ] Hot code reloading
- [ ] JIT compilation
- [ ] LLVM backend for native code generation

---

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 🏆 Credits

**Project**: EPITECH B-FUN-500 (GLaDOS)

**Development Team**: GLaDOS Contributors

**Technologies**:
- **Language**: Haskell (GHC 9.10.2)
- **Build Tool**: Stack
- **Parser**: Megaparsec
- **Testing**: hspec + hspec-discover
- **Concurrency**: STM (Software Transactional Memory)
- **CI/CD**: GitHub Actions

---

## 📞 Support

### Getting Help

- 📖 Read the [documentation](docs/)
- 💬 Check existing [issues](https://github.com/your-username/Glados-On-Top/issues)
- 🐛 Report bugs via [GitHub Issues](https://github.com/your-username/Glados-On-Top/issues/new)

### Useful Commands

```bash
# Get help
./glados --help
./glados-vm --help

# Build from scratch
make fclean && make build

# Run all checks
make format-check && make hlint && make tests_run

# View coverage
make coverage
open $(stack path --local-hpc-root)/index.html
```

---

## 🎓 Educational Value

This project demonstrates:

✅ **Language Design**: Formal grammar, type systems, semantics
✅ **Compiler Construction**: Parsing, AST, bytecode generation
✅ **Virtual Machines**: Stack-based execution, instruction sets
✅ **Concurrency**: Actor model, message passing, STM
✅ **Type Safety**: Strong typing, memory safety, error handling
✅ **Software Engineering**: Testing, CI/CD, documentation
✅ **Functional Programming**: Haskell, pure functions, immutability

---

## 📸 Example Session

```bash
$ cat examples/advanced/Calculator.rat
proc Calculator() {
  state: 0,
  receive {
    | (:add, x) -> state = state + x
    | (:mul, x) -> state = state * x
    | (:get, sender) -> sender <- state
    | :clear -> state = 0
  }
}

proc main() {
  let calc = spawn Calculator()
  calc <- (:add, 10)
  calc <- (:mul, 5)
  calc <- (:get, self)
}

$ ./glados examples/advanced/Calculator.rat -o calc.rtbc
Compilation successful: calc.rtbc

$ ./glados-vm calc.rtbc
50
```

---

## 🌟 Acknowledgments

Special thanks to:
- **EPITECH** for the project framework
- **Haskell Community** for excellent libraries and tools
- **Erlang/Elixir Community** for actor model inspiration
- **Rust Community** for safety-first design philosophy

---

**Built with ❤️ and Haskell**

*"The cake is NOT a lie"* - GLaDOS
