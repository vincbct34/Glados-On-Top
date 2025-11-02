# GLaDOS - Ratatouille Language

**GLaDOS** (Generic Language and Data Operand Syntax) is a functional programming language with actor-model concurrency, featuring a compiler, bytecode virtual machine, and pattern matching capabilities.

## Overview

Ratatouille is a modern language that combines:
- **Pure functional programming** with first-class functions
- **Actor-based concurrency** with lightweight processes and message passing
- **Pattern matching** for control flow and message handling
- **Type safety** with static and dynamic typing support
- **Bytecode compilation** for efficient execution

## Quick Start

### Prerequisites

- **Haskell Stack** (for building)
- **GNU Make**

### Building

```bash
make           # Build the compiler and VM
make re        # Clean rebuild
make clean     # Remove build artifacts
make fclean    # Remove binaries
```

This produces two executables:
- `glados` - The compiler
- `glados-vm` - The virtual machine

### Running Your First Program

```bash
# Create a simple program
cat > hello.rat << 'EOF'
proc main() {
    print("Hello, World!")
}
EOF

# Compile and run
./glados hello.rat          # Compiles to hello.rtbc
./glados-vm hello.rtbc      # Runs the bytecode
```

## Language Features

### Processes and Concurrency

Define actor processes that communicate via message passing:

```ratatouille
proc Counter() {
    state: 0,
    receive {
        | :increment -> state = state + 1
        | :get -> print(state)
    }
}

proc main() {
    let counter = spawn Counter()
    counter <- :increment
    counter <- :get
}
```

### Pattern Matching

Pattern match on messages, tuples, and values:

```ratatouille
proc Echo() {
    receive {
        | (:hello, name) -> print("Hello, " + name)
        | (:goodbye, _)  -> print("Goodbye!")
        | msg            -> print(msg)
    }
}
```

### Pure Functions

```ratatouille
proc factorial(n) {
    if n <= 1 then 1
    else n * factorial(n - 1)
}
```

### Type System

- Primitives: `Int`, `Float`, `String`, `Bool`, `Atom`
- Collections: `Array`, `Tuple`
- Special types: `Maybe`, `Either`, `Void`

### Module System

```ratatouille
import Counter from "utils.rat"
import {Helper, Logger} from "lib.rat"
```

## Compiler Usage

```bash
# Compile to bytecode
./glados program.rat

# Specify output file
./glados program.rat -o output.rtbc

# Show bytecode
./glados program.rat --show-bytecode

# Show AST
./glados program.rat --show-ast

# Inspect binary file
./glados compiled.rtbc --inspect
```

## Virtual Machine Usage

```bash
# Run bytecode
./glados-vm program.rtbc

# Run with debugging
./glados-vm --debug program.rtbc

# Run with instruction tracing
./glados-vm --trace program.rtbc
```

## Examples

The `examples/` directory contains sample programs:

**Basics:**
- `examples/basics/helloWorld.rat` - Hello World
- `examples/basics/counter.rat` - Simple counter
- `examples/basics/Variables.rat` - Variable bindings
- `examples/basics/Arrays.rat` - Array operations

**Advanced:**
- `examples/advanced/asynchroneCalc.rat` - Async calculations
- `examples/advanced/recursiveCounter.rat` - Recursive processes
- `examples/advanced/triangularComm.rat` - Multi-process communication
- `examples/advanced/MaybeEither.rat` - Maybe/Either monads
- `examples/advanced/Router.rat` - Message routing

**Modules:**
- `examples/modules/utils.rat` - Reusable utilities

Run all examples:
```bash
./test_all_examples.sh
```

## Testing

```bash
make test              # Run test suite
./test_all_examples.sh # Test all example programs
```

## Documentation

Comprehensive documentation is available in the `docs/` directory:

- **[Language Reference](docs/LANGUAGE_REFERENCE.md)** - Complete syntax guide
- **[Quick Start Guide](docs/QUICK_START.md)** - Get started in 5 minutes
- **[Documentation Index](docs/INDEX.md)** - Complete listing

Feature guides:
- [Type System](docs/TYPE_SYSTEM_GUIDE.md)
- [Arrays & Tuples](docs/ARRAYS_AND_TUPLES_GUIDE.md)
- [Concurrency Model](docs/NEXUS_CONCEPT.md)

## Project Structure

```
.
├── src/                    # Haskell source code
│   ├── Ratatouille/
│   │   ├── AST.hs         # Abstract syntax tree
│   │   ├── Parser/        # Parser modules
│   │   ├── Bytecode/      # Compiler and bytecode types
│   │   └── VM/            # Virtual machine
├── app/                    # Executable entry points
│   ├── Main.hs            # Compiler (glados)
│   ├── compiler/          # Alternative compiler
│   └── vm/                # VM (glados-vm)
├── examples/              # Example programs
├── docs/                  # Documentation
└── test/                  # Test suite
```

## Language Syntax Highlights

```ratatouille
// Pure function
proc double(x) {
    x * 2
}

// Actor process with state
proc StatefulCounter() {
    state: 0,
    receive {
        | :inc     -> state = state + 1
        | (:set, n) -> state = n
        | :get     -> print(state)
    }
}

// Main entry point
proc main() {
    // Variables
    let x = 42
    const PI = 3.14

    // Arrays and tuples
    let arr = [1, 2, 3]
    let tuple = (x, "hello", true)

    // Spawn process
    let counter = spawn StatefulCounter()

    // Send messages
    counter <- :inc
    counter <- (:set, 10)
    counter <- :get
}
```

## License

Educational project developed at EPITECH.

## Contributing

See the [Developer Guide](docs/DEVELOPER_GUIDE.md) for contribution guidelines.
