##
## EPITECH PROJECT, 2025
## Bootstrap Wolfram
## File description:
## Makefile
##

COMPILER_NAME 	=	Glados-On-Top-exe
VM_NAME 		=	Glados-VM-exe

AT_COMPILER =	glados
AT_VM       =	glados-vm

# Build targets
build: 		stack
			cp $(shell stack path --local-install-root)/bin/$(COMPILER_NAME) .
			mv $(COMPILER_NAME) $(AT_COMPILER)
			cp $(shell stack path --local-install-root)/bin/$(VM_NAME) .
			mv $(VM_NAME) $(AT_VM)

all: 		build

stack:
			stack build

# Development targets
dependencies:
			stack build --only-dependencies

fast-build:
			stack build --fast

install:
			stack install --local-bin-path ./dist

# Release build: optimized binary for packaging
release-build:
			@echo "Building optimized release binaries..."
			stack build --copy-bins --ghc-options="-O2"
			mkdir -p dist
			SRC=$$(stack path --local-bin)/$(COMPILER_NAME); \
			if [ -f "$$SRC" ]; then \
				cp "$$SRC" dist/$(AT_COMPILER); \
				strip dist/$(AT_COMPILER) || true; \
				( cd dist && sha256sum $(AT_COMPILER) > $(AT_COMPILER).sha256 || true ); \
			else \
				echo "Executable introuvable: $$SRC" >&2; exit 1; \
			fi
			SRC=$$(stack path --local-bin)/$(VM_NAME); \
			if [ -f "$$SRC" ]; then \
				cp "$$SRC" dist/$(AT_VM); \
				strip dist/$(AT_VM) || true; \
				( cd dist && sha256sum $(AT_VM) > $(AT_VM).sha256 || true ); \
			else \
				echo "Executable introuvable: $$SRC" >&2; exit 1; \
			fi

package-release: release-build
			@echo "Release package prepared in dist/"

# Test targets
tests_run:
			stack test

coverage:
			stack test --coverage
			stack hpc report --all --destdir coverage/
			mv coverage/hpc_index.html coverage/index.html
			@echo "Coverage reports generated."
			@echo "View the unified report at: $$(stack path --local-hpc-root)/combined/all/index.html"
			@echo "View the index of all reports at: $$(stack path --local-hpc-root)/index.html"

# Code quality targets
hlint:
			@echo "Running HLint..."
			@which hlint > /dev/null || (echo "Installing HLint..." && stack install hlint)
			hlint src/ app/ --report=hlint-report.html

format-check:
			@echo "Checking code formatting..."
			@if ! which ormolu > /dev/null 2>&1; then \
				echo "Ormolu not found. Installing..."; \
				stack install ormolu; \
			fi
			@echo "Running Ormolu format check..."
			@if which ormolu > /dev/null 2>&1; then \
				ormolu --mode check $$(find . -name '*.hs' -not -path './.stack-work/*') || echo "Code formatting issues found. Run 'make format' to fix them."; \
			else \
				echo "Warning: Ormolu not available, skipping format check"; \
			fi

format:
			@echo "Formatting code..."
			@if ! which ormolu > /dev/null 2>&1; then \
				echo "Ormolu not found. Installing..."; \
				stack install ormolu; \
			fi
			@if which ormolu > /dev/null 2>&1; then \
				ormolu --mode inplace $$(find . -name '*.hs' -not -path './.stack-work/*'); \
				echo "Code formatting completed."; \
			else \
				echo "Error: Failed to install Ormolu"; \
				exit 1; \
			fi

# CI targets (combines multiple checks)
ci-build: dependencies fast-build

ci-test: coverage

ci-quality: hlint format-check

ci-all: ci-build ci-test ci-quality

# Clean targets
clean:
			stack clean

fclean: 	clean
			stack clean --full
			rm -f $(AT_COMPILER)
			rm -f $(AT_VM)
			rm -f hlint-report.html
			rm -rf dist/
			find . -name "*.gbc" -or -name "*.rtbc" -delete

re: 		fclean all

# Help target
help:
			@echo "Available targets:"
			@echo "  build        - Build the project and create executable"
			@echo "  all          - Same as build"
			@echo "  stack        - Build with stack only"
			@echo "  dependencies - Install dependencies only"
			@echo "  fast-build   - Fast build without optimizations"
			@echo "  install      - Install executable to ./dist"
			@echo "  tests_run    - Run tests"
			@echo "  coverage     - Run tests with coverage"
			@echo "  hlint        - Run HLint code analysis"
			@echo "  format-check - Check code formatting"
			@echo "  format       - Format code with Ormolu"
			@echo "  ci-build     - CI build target"
			@echo "  ci-test      - CI test target"
			@echo "  ci-quality   - CI quality checks"
			@echo "  ci-all       - Run all CI checks"
			@echo "  clean        - Clean build artifacts"
			@echo "  fclean       - Full clean"
			@echo "  re           - Rebuild from scratch"
			@echo "  help         - Show this help"

.PHONY: all build stack dependencies fast-build install tests_run coverage hlint format-check format ci-build ci-test ci-quality ci-all clean fclean re help
