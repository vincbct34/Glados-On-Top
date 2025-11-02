# Ratatouille Language Support for VS Code

Full-featured VS Code extension for the Ratatouille language (.rat). Ratatouille is an actor-model-based programming language developed as part of the Glados project.

## ✨ Features

### 🎨 Advanced Syntax Highlighting
- Full keyword support: `proc`, `func`, `receive`, `spawn`, `state`, `let`, `const`, `if`, `then`, `else`, `match`, `import`, and more.
- Numeric types recognized: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`
- Advanced types: `Maybe`, `Either`, `Array`, `Tuple`, `Pid`
- Atoms (e.g. `:atom_name`)
- Comments using `//` and `#`

### 💡 IntelliSense & Autocompletion
- Smart suggestions for:
  - `proc` and `func` definitions
  - Variables and parameters
  - Language keywords
  - Data types
  - Atoms already used in the file

### 🔍 Hover Information
Hover any symbol to see documentation and quick info:
- Procs: signature and description
- Functions: signature and return type
- Variables: inferred type and docstring
- Keywords: short syntax explanation

### 🎯 Go to Definition
Jump to definitions for:
- Processes (`proc`)
- Functions (`func`)
- Variables and parameters

### ⚡ Snippets
Over 30 code snippets to speed up development. Examples include:

| Prefix | Description |
|--------|-------------|
| `proc` | Full process definition with state |
| `func` | Pure function definition |
| `main` | Main entry point |
| `receive` | Message receive block |
| `if` | If-then-else expression |
| `match` | Pattern matching expression |
| `spawn` | Spawn a new process |
| `send` | Send a message |
| `counter` | Complete counter pattern |
| `import-*` | Various import styles |
| … | And more |

### 🔧 Editor Configuration
- Auto-closing of braces, parentheses and brackets
- Smart indentation
- Word pattern configured for the language

## 📦 Installation

### From VSIX
```bash
cd bonus-linter
npm install
npm run compile
npm run package
code --install-extension ratatouille-language-support-2.0.0.vsix
```

### Development
```bash
cd bonus-linter
npm install
npm run compile
# Then press F5 in VS Code to launch the extension in debug mode
```

## 🚀 Usage

1. Open a `.rat` file
2. The extension activates automatically
3. Use IntelliSense, hover, and go-to-definition features

### Code Examples

#### Simple Process
```ratatouille
proc Greeter() {
    receive {
        | (:hello, sender) -> print("Hello!")
        | (:goodbye, sender) -> print("Goodbye!")
    }
}
```

#### Stateful Counter
```ratatouille
proc Counter(initial) {
    state: initial,
    receive {
        | :increment -> state = state + 1
        | :decrement -> state = state - 1
        | (:get, sender) -> sender <- state
    }
}
```

#### Pure Function
```ratatouille
func factorial(n) {
    if n <= 1 then
        1
    else
        n * factorial(n - 1)
}
```

## ⚙️ Configuration

Open settings at `Preferences > Settings > Extensions > Ratatouille` and configure:

- `ratatouille.linter.enabled` : Enable/disable the linter (default: `true`)
- `ratatouille.linter.gladosPath` : Path to the Glados executable (optional)
- `ratatouille.linter.maxProblems` : Maximum number of reported problems (default: `100`)

## 🏗️ Architecture

```
bonus-linter/
├── src/
│   ├── extension.ts     # Extension entry point
│   ├── server.ts        # Language Server Protocol server
│   └── analyzer.ts      # .rat document analysis
├── syntaxes/
│   └── ratatouille.tmLanguage.json  # TextMate grammar
├── snippets/
│   └── ratatouille.json # Code snippets
├── language-configuration.json  # Language configuration
└── package.json         # Extension manifest
```

## 📝 Ratatouille Syntax

### Key Keywords
- `proc` : Define a process (actor)
- `func` : Define a pure function
- `receive` : Message receive block
- `spawn` : Create a new process
- `state` : Internal state of a process
- `let` / `const` : Variable declarations
- `self` : PID of the current process
- `if then else` : Conditional expressions
- `match` : Pattern matching
- `import from` : Import from a module

### Types
- Numeric: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`
- Advanced: `Maybe`, `Either`, `Array`, `Tuple`, `Pid`, `Bool`, `String`

### Operators
- Message send: `<-`
- Arrow: `->`
- Monad bind: `>>=`
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `!`

## 🔗 Useful Links

- [GitHub Repository](https://github.com/vincbct34/Glados-On-Top)
- [Ratatouille Documentation](https://github.com/vincbct34/Glados-On-Top/tree/main/docs)

## 🐛 Report a Bug

Please open an issue on [GitHub Issues](https://github.com/vincbct34/Glados-On-Top/issues)

## 📜 License

See [LICENSE](../LICENSE)

## 👥 Authors

Developed as part of the Glados project - EPITECH 2025

---

Enjoy coding in Ratatouille! 🐀🍳
