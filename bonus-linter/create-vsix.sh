#!/bin/bash

# Script pour créer le package VSIX avec Node 20
# Pour la publication sur VS Code Marketplace

set -e

echo "📦 Création du Package VSIX pour VS Code Marketplace"
echo "===================================================="
echo ""

# Vérifier qu'on est dans le bon dossier
if [ ! -f "package.json" ]; then
    echo "❌ Erreur: Ce script doit être exécuté depuis le dossier bonus-linter"
    exit 1
fi

# Sauvegarder la version Node actuelle
CURRENT_NODE=$(node -v)
echo "📍 Version Node actuelle: $CURRENT_NODE"
echo ""

# Vérifier si nvm est installé
if ! command -v nvm &> /dev/null; then
    echo "📥 Installation de nvm (Node Version Manager)..."
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.0/install.sh | bash
    
    # Charger nvm
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    
    echo "✅ nvm installé"
    echo ""
fi

# Charger nvm si pas déjà chargé
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# Installer Node 20 si pas déjà installé
echo "📥 Installation de Node 20..."
nvm install 20
echo "✅ Node 20 installé"
echo ""

# Utiliser Node 20
echo "🔄 Passage à Node 20..."
nvm use 20
echo "✅ Maintenant sur Node $(node -v)"
echo ""

# Installer vsce si nécessaire
if ! command -v vsce &> /dev/null; then
    echo "📦 Installation de vsce..."
    npm install -g @vscode/vsce
    echo "✅ vsce installé"
    echo ""
fi

# Vérifier que la compilation est à jour
if [ ! -d "out" ] || [ ! -f "out/extension.js" ]; then
    echo "🔨 Compilation de l'extension..."
    npm run compile
    echo "✅ Compilation terminée"
    echo ""
fi

# Créer le package VSIX
echo "📦 Création du package VSIX..."
vsce package

# Vérifier que le fichier a été créé
if [ -f *.vsix ]; then
    VSIX_FILE=$(ls *.vsix)
    echo ""
    echo "✅ Package créé avec succès !"
    echo ""
    echo "📄 Fichier: $VSIX_FILE"
    echo "📊 Taille: $(du -h "$VSIX_FILE" | cut -f1)"
    echo ""
    echo "🎯 Prochaines étapes:"
    echo "1. Aller sur: https://marketplace.visualstudio.com/manage"
    echo "2. Cliquer sur 'New Extension' → 'Visual Studio Code'"
    echo "3. Drag & drop le fichier: $VSIX_FILE"
    echo "4. Remplir les informations et publier !"
    echo ""
    echo "💡 Ou publier directement avec:"
    echo "   vsce publish"
    echo ""
else
    echo "❌ Erreur: Le fichier VSIX n'a pas été créé"
    exit 1
fi

# Optionnel : Revenir à la version Node précédente
read -p "Voulez-vous revenir à Node $CURRENT_NODE ? (o/N) " -n 1 -r
echo
if [[ $REPLY =~ ^[OoYy]$ ]]; then
    nvm use system 2>/dev/null || nvm use default 2>/dev/null || true
    echo "✅ Retour à Node $(node -v)"
fi

echo ""
echo "🎉 Terminé !"
