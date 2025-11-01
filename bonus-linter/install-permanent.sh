#!/bin/bash

# Script d'installation permanente de l'extension Ratatouille
# Pour VS Code

set -e

echo "🐀 Installation Permanente - Extension Ratatouille"
echo "=================================================="
echo ""

# Vérifier qu'on est dans le bon dossier
if [ ! -f "package.json" ]; then
    echo "❌ Erreur: Ce script doit être exécuté depuis le dossier bonus-linter"
    exit 1
fi

# Vérifier que VS Code est installé
if ! command -v code &> /dev/null; then
    echo "❌ VS Code n'est pas installé ou 'code' n'est pas dans le PATH"
    exit 1
fi

# Définir le dossier des extensions VS Code
VSCODE_EXT_DIR="$HOME/.vscode/extensions"
EXT_NAME="glados-team.ratatouille-language-support-1.0.0"
EXT_PATH="$VSCODE_EXT_DIR/$EXT_NAME"

echo "📦 Méthode d'installation choisie: Copie dans le dossier extensions"
echo "Destination: $EXT_PATH"
echo ""

# Créer le dossier extensions s'il n'existe pas
mkdir -p "$VSCODE_EXT_DIR"

# Supprimer TOUTES les anciennes versions
echo "🗑️  Suppression des anciennes versions..."
rm -rf "$VSCODE_EXT_DIR"/ratatouille-language-support*
rm -rf "$VSCODE_EXT_DIR"/*ratatouille-language-support*

# Vérifier que la compilation est à jour
if [ ! -d "out" ] || [ ! -f "out/extension.js" ]; then
    echo "🔨 Compilation de l'extension..."
    npm run compile
fi

# Copier l'extension
echo "📋 Copie des fichiers de l'extension..."
mkdir -p "$EXT_PATH"

# Copier uniquement les fichiers nécessaires
cp package.json "$EXT_PATH/"
cp language-configuration.json "$EXT_PATH/"
cp -r out "$EXT_PATH/"
cp -r syntaxes "$EXT_PATH/"
cp -r icons "$EXT_PATH/"
cp README.md "$EXT_PATH/"
cp CHANGELOG.md "$EXT_PATH/"

# Copier node_modules (nécessaire pour vscode-languageclient, etc.)
if [ -d "node_modules" ]; then
    echo "📦 Copie des dépendances..."
    cp -r node_modules "$EXT_PATH/"
fi

echo ""
echo "✅ Extension installée avec succès !"
echo ""
echo "📝 Prochaines étapes:"
echo "  1. Redémarrez VS Code complètement"
echo "  2. Ouvrez n'importe quel fichier .rat"
echo "  3. Vérifiez en bas à droite que le langage est 'Ratatouille'"
echo ""
echo "🔍 Pour vérifier l'installation:"
echo "  code --list-extensions | grep ratatouille"
echo ""
echo "🗑️  Pour désinstaller:"
echo "  rm -rf $EXT_PATH"
echo ""

# Proposer de redémarrer VS Code
read -p "Voulez-vous que je ferme toutes les fenêtres VS Code ? (o/N) " -n 1 -r
echo
if [[ $REPLY =~ ^[OoYy]$ ]]; then
    echo "🔄 Fermeture de VS Code..."
    killall code 2>/dev/null || true
    sleep 1
    echo "✅ Vous pouvez maintenant relancer VS Code"
else
    echo "⚠️  N'oubliez pas de redémarrer VS Code manuellement !"
fi

echo ""
echo "🎉 Installation terminée !"
