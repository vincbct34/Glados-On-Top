#!/bin/bash

# Installation permanente de l'extension Ratatouille Language Support v2.0
# Ce script installe l'extension dans VS Code

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

echo "🐀 Installation de l'extension Ratatouille Language Support v2.0"
echo "================================================================="
echo ""

# Vérifier que Node.js est installé
if ! command -v node &> /dev/null; then
    echo "❌ Node.js n'est pas installé."
    echo "   Installez Node.js 18+ depuis https://nodejs.org/"
    exit 1
fi

echo "✅ Node.js version: $(node --version)"

# Vérifier que VS Code est installé
if ! command -v code &> /dev/null; then
    echo "❌ VS Code CLI n'est pas disponible."
    echo "   Installez VS Code et activez le dans le PATH:"
    echo "   VS Code → Ctrl+Shift+P → 'Shell Command: Install code command in PATH'"
    exit 1
fi

echo "✅ VS Code installé"
echo ""

# Installer les dépendances
echo "📦 Installation des dépendances..."
npm install

# Compiler le TypeScript
echo ""
echo "🔨 Compilation du code TypeScript..."
npm run compile

# Créer le package VSIX
echo ""
echo "📦 Création du package VSIX..."
npm run package

# Trouver le fichier VSIX créé
VSIX_FILE=$(ls -t *.vsix 2>/dev/null | head -1)

if [ -z "$VSIX_FILE" ]; then
    echo "❌ Erreur: Fichier VSIX non créé"
    exit 1
fi

echo "✅ Package créé: $VSIX_FILE"
echo ""

# Installer l'extension
echo "🚀 Installation de l'extension dans VS Code..."
code --install-extension "$VSIX_FILE" --force

echo ""
echo "================================================================="
echo "✅ Installation terminée avec succès!"
echo ""
echo "📋 Pour activer l'extension:"
echo "   1. Rechargez VS Code: Ctrl+Shift+P → 'Reload Window'"
echo "   2. Ouvrez un fichier .rat"
echo "   3. L'extension s'activera automatiquement"
echo ""
echo "💡 Fonctionnalités disponibles:"
echo "   • Coloration syntaxique avancée"
echo "   • Hover pour documentation (survolez les symboles)"
echo "   • Go to Definition (F12 ou Ctrl+Click)"
echo "   • Auto-complétion (Ctrl+Space)"
echo "   • 30+ snippets de code (tapez 'proc', 'func', etc.)"
echo ""
echo "🔍 Pour tester:"
echo "   cd ../examples/basics"
echo "   code counter.rat"
echo ""
echo "🐛 En cas de problème:"
echo "   • Vérifiez Output → Ratatouille Language Server"
echo "   • Voir UPGRADE_GUIDE.md pour le dépannage"
echo ""
echo "Enjoy coding in Ratatouille! 🐀🍳"
