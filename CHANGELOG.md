# Changelog

All notable changes to the **Ratatouille** programming language (GLaDOS project) will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [3.0.0] - 2025-11-02

### Major Refactoring Release
This version represents a complete overhaul of the compiler, parser, and virtual machine with significant improvements in code quality, error handling, and language features.

### Added
- **Modulo operator (%)**: Full support for modulo arithmetic operations
  - Parser support for `%` symbol with multiplicative precedence
  - Bytecode `MOD` instruction (opcode 0x34)
  - VM interpreter support with division-by-zero checking
  - Example: `17 % 5` returns `2`
- **Multiple main() warning system**: Warns users when multiple `main()` functions are defined
  - Displays: `"Warning: Found N main() definitions. Using the last one."`
  - Compiler uses the last defined `main()` function
- **MCP Server for Ratatouille**: Language server integration for IDE support
  - Document validation with diagnostics
  - Undefined reference detection
  - Unused variable warnings
  - Restart command support
  - Enhanced logging
- **Comprehensive test script**: `test_all_examples.sh` for testing all 52 example programs
- **Syntax documentation**: Complete `RATATOUILLE_SYNTAX.md` reference guide
- **Binary bytecode format**: `.rtbc` file format for compiled programs
  - Bytecode encoder/decoder with opcodes
  - File format specification

### Changed
- **Parser refactoring**: Complete modularization of parser components
  - Split into `Common.hs`, `ExprStmt.hs`, `Pattern.hs`, and `Proc.hs`
  - Improved operator precedence handling
  - Better error messages with context extraction
  - Fixed process body parsing (removed duplicate `receive` keyword bug)
- **Compiler improvements**: Enhanced Main.hs with better error handling
  - Recursive import resolution
  - Import filtering (all, selected, single)
  - Rich error formatting with source context
  - Bytecode inspection mode (`--inspect`)
  - Multiple output modes (show-bytecode, show-ast, write-binary)
- **VM enhancements**: Refactored runtime and interpreter
  - Improved argument parsing
  - Better debug and trace modes
  - Enhanced error messages
  - Fixed function definition handling
- **Project restructuring**: Split `app/` into `compiler/` and `vm/` subdirectories
- **Cross-platform support**: Removed Windows-specific commands from Makefile
- **Grammar updates**: Renamed `nexus.bnf` to `ratatouille.bnf`
- **Documentation cleanup**: Removed outdated `NEXUS_CONCEPT.md` and `PROJECT_STRATEGY.md`

### Fixed
- **Process parsing bug**: Fixed duplicate `receive` keyword consumption in `procWithoutState`
  - Processes with only receive blocks (no explicit state) now parse correctly
  - Restored 51/52 passing tests (from 42/52)
- **Multi-pattern receive**: Fixed pattern matching bug in receive blocks
- **String concatenation**: Now handles all value types (not just strings)
- **Pure function support**: Proper handling of `fn main()` syntax
- **Import system**: Correct resolution of recursive imports

### Documentation
- Added comprehensive Haddock documentation for Parser and VM modules
- Security analysis documentation (`SECURITY_ANALYSIS.md`)
- Architecture documentation improvements
- Project status tracking with completed features and limitations

---

## [2.0.3] - 2025-11-02

### Changed
- Fixed `node_modules` inclusion in VSIX package
- Cleaned up activation messages in language server

---

## [2.0.1] - 2025-11-01

### Added
- Document validation with diagnostics for undefined references
- Unused variable detection
- Restart command for language server
- Enhanced logging system

---

## [2.0.0] - 2025-11-01

### Added
- **Ratatouille Language Support Extension**: VS Code extension with syntax highlighting and linting
  - Language grammar and tokenization
  - Code completion
  - Error diagnostics
  - Hover information

---

## [1.1.0] - 2025-10-25

### Added
- **Coverage reporting**: Automated HPC coverage report generation
- GitHub Actions workflow for coverage report deployment to GitHub Pages
- Coverage report in Makefile (`make coverage`)

### Changed
- Code formatting with Ormolu for consistency
- Import reordering for better readability

---

## [1.0.0] - 2025-10-30

### Initial Major Release

### Added
- **Core Language Features**:
  - Actor-model concurrency with process spawning and message passing
  - Pattern matching in receive blocks
  - Type system with 10 numeric types (i8, i16, i32, i64, u8, u16, u32, u64, f32, f64)
  - Maybe and Either types for safe error handling
  - Tuples and arrays
  - Type casting (safe, unsafe, const)
  - Unary operations (!, -, +)
  - Pre/post increment and decrement operators (++x, x++, --x, x--)
  - String concatenation
  - Match expressions for pattern matching on values
  - Import system with recursive module loading

- **Compiler**:
  - Megaparsec-based parser with full language support
  - AST representation with comprehensive type definitions
  - Bytecode compiler with 65+ instructions
  - Binary encoder for `.rtbc` format
  - Import resolution with filtering (all, selected, single)

- **Virtual Machine**:
  - Stack-based bytecode interpreter
  - STM-based concurrent process management
  - Process state management (INIT_STATE, GET_STATE, SET_STATE)
  - Message passing with mailboxes
  - Arithmetic operations (ADD, SUB, MUL, DIV)
  - Comparison operations (EQ, NEQ, LT, GT, LTE, GTE)
  - Logical operations (AND, OR)
  - Array operations (INDEX, ARRAY_LENGTH, PUSH_ARRAY)
  - Tuple operations (PUSH_TUPLE, GET_FIELD)
  - Maybe/Either support (PUSH_JUST, PUSH_NONE, PUSH_LEFT, PUSH_RIGHT)
  - Function and process definitions (DEFINE_FUNCTION, DEFINE_PROCESS)

- **Testing**:
  - 4,949 lines of comprehensive tests across 13 test files
  - Unit tests for Parser, AST, Bytecode, VM, Interpreter, Runtime
  - Integration tests for end-to-end compilation and execution
  - 52 example programs demonstrating language features
  - HSpec test framework with auto-discovery

- **Build System**:
  - Stack-based Haskell build
  - Makefile with targets: build, test, coverage, format, hlint, clean
  - Cross-platform support (Linux, macOS, Windows)

- **Documentation**:
  - Formal EBNF grammar specification
  - Language reference guide
  - Architecture documentation
  - Security analysis
  - Bytecode architecture specification
  - Developer guide (CLAUDE.md)
  - Comprehensive README.md

- **Examples**:
  - Basic examples: Hello World, counters, variables, conditionals
  - Advanced examples: Calculator, Router, async operations, error handling
  - Module system examples with imports
  - Debug examples for troubleshooting

### Technical Details
- **Language**: Haskell (GHC 9.10.2)
- **Build Tool**: Stack
- **Parser**: Megaparsec
- **Testing**: hspec + hspec-discover
- **Concurrency**: STM (Software Transactional Memory)
- **Code Quality**: HLint, Ormolu formatting

---

## [Unreleased]

### Planned Features
- Tail call optimization (TCO)
- Bytecode disassembler
- File I/O operations
- Standard library
- Module system enhancements
- Generic types (parametric polymorphism)
- Supervisor trees for fault tolerance
- Hot code reloading
- JIT compilation
- LLVM backend for native code generation

---

## Version History Summary

| Version | Date | Description |
|---------|------|-------------|
| 3.0.0 | 2025-11-02 | Major refactoring: modulo operator, parser fixes, MCP server |
| 2.0.3 | 2025-11-02 | VS Code extension fixes |
| 2.0.1 | 2025-11-01 | Document validation and diagnostics |
| 2.0.0 | 2025-11-01 | VS Code extension release |
| 1.1.0 | 2025-10-25 | Coverage reporting and CI/CD |
| 1.0.0 | 2025-10-30 | Initial major release |

---

## Contributing

See [CLAUDE.md](CLAUDE.md) for development workflow and contribution guidelines.

## License

MIT License - See [LICENSE](LICENSE) for details.
