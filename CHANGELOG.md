# Changelog for Glados-On-Top

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.0.1.0 - 2025-09-17

### Added
- Initial project scaffolding: cabal/stack configuration, main executable skeleton, and core modules (`Builtins`, `Env`, `Errors`, `Eval`, `Lexer`, `Parser`, `Types`).
- Core evaluation pipeline: lexical analysis, parsing, environment management, built‑ins, error handling, and type definitions.
- CI/CD pipeline (build, test, lint, mirror, release) with safeguards to avoid executing on the official Epitech mirror.
- GitHub Actions mirror & repository check steps.
- HLint configuration for code quality.
- Property / behavior test suite using `hspec`.
- Added extra Stack dependency: `cabal-syntax-3.14.2.0`.
- Documentation: comprehensive `README` (usage, development workflow, quality gates) and file/module header comments.
- Makefile targets for build, test, lint, coverage, and helper tasks.

### Changed
- Refined `README` to better describe goals and development guidelines.
- Standardized quotation style and formatting in CI workflows.
- Streamlined CI workflow: consolidated jobs, enforced `main` triggers, added coverage reporting.
- Minor style tweak in `Setup.hs` (newline + header comment).
- Expanded Makefile with additional convenience and quality targets.

### Fixed
- Corrected executable naming and improved error handling in CI workflow.
- Makefile updated to use a variable for the executable name to avoid hardcoding.

### Removed
- Removed accidentally committed `glados` binary and added it to `.gitignore`.

### Notes
- First (pre‑alpha) public release.