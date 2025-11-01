#!/bin/bash

# Build script for Ratatouille VSCode Extension
# Usage: ./build.sh [--clean] [--install]

set -e  # Exit on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

echo "🐀 Ratatouille Extension Build Script"
echo "======================================"

# Parse arguments
CLEAN=false
INSTALL=false

for arg in "$@"; do
    case $arg in
        --clean)
            CLEAN=true
            shift
            ;;
        --install)
            INSTALL=true
            shift
            ;;
        *)
            echo "Unknown option: $arg"
            echo "Usage: ./build.sh [--clean] [--install]"
            exit 1
            ;;
    esac
done

# Clean if requested
if [ "$CLEAN" = true ]; then
    echo ""
    echo "🧹 Cleaning build artifacts..."
    rm -rf node_modules out *.vsix
    echo "✅ Clean complete"
fi

# Check for Node.js
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed. Please install Node.js 18+ first."
    exit 1
fi

echo ""
echo "📦 Installing dependencies..."
npm install

echo ""
echo "🔨 Compiling TypeScript..."
npm run compile

echo ""
echo "✅ Build complete!"

# Package the extension
echo ""
echo "📦 Creating VSIX package..."
npm run package

VSIX_FILE=$(ls -t *.vsix 2>/dev/null | head -1)

if [ -z "$VSIX_FILE" ]; then
    echo "❌ VSIX file not created"
    exit 1
fi

echo "✅ Created: $VSIX_FILE"

# Install if requested
if [ "$INSTALL" = true ]; then
    echo ""
    echo "🚀 Installing extension..."
    code --install-extension "$VSIX_FILE" --force
    echo "✅ Extension installed!"
    echo ""
    echo "Please reload VS Code to activate the extension."
    echo "You can do this with: Ctrl+Shift+P → 'Reload Window'"
fi

echo ""
echo "======================================"
echo "🎉 Build complete!"
echo ""
echo "To install manually:"
echo "  code --install-extension $VSIX_FILE"
echo ""
echo "Or from VS Code:"
echo "  Extensions → ... → Install from VSIX → Select $VSIX_FILE"
