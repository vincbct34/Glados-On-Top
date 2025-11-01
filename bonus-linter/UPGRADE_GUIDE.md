# Guide de Mise à Jour - Extension Ratatouille v2.0

## 🎉 Nouvelles Fonctionnalités

### Version 2.0.0 (Novembre 2024)

Cette mise à jour majeure transforme l'extension d'un simple highlighting en un véritable environnement de développement intégré pour Ratatouille.

#### ✨ Nouveautés

1. **Language Server Protocol (LSP)**
   - Serveur de langage complet avec analyse en temps réel
   - Architecture modulaire avec `extension.ts`, `server.ts`, et `analyzer.ts`

2. **Hover Information**
   - Documentation contextuelle au survol
   - Signatures de fonctions et processus
   - Types de variables
   - Documentation des mots-clés

3. **Go to Definition**
   - Navigation vers les définitions de `proc`
   - Navigation vers les définitions de `func`
   - Navigation vers les déclarations de variables

4. **IntelliSense Avancé**
   - Autocomplétion intelligente basée sur le contexte
   - Suggestions de procs, funcs, variables
   - Suggestions d'atoms utilisés dans le fichier
   - Suggestions de mots-clés et types

5. **Snippets Complets**
   - Plus de 30 snippets pour toutes les constructions du langage
   - Templates pour proc, func, receive, match, etc.
   - Patterns courants (counter, etc.)

6. **Syntaxe Améliorée**
   - Support de `func` en plus de `proc`
   - Tous les mots-clés : if, then, else, match, import, from
   - Types numériques : i8, i16, i32, i64, u8, u16, u32, u64, f32, f64
   - Types avancés : Maybe, Either, Array, Tuple
   - Opérateurs monadiques : >>=
   - Commentaires // et #

7. **Configuration du Langage**
   - Auto-closing intelligent
   - Indentation contextuelle
   - Règles onEnter personnalisées

## 📦 Installation de la Nouvelle Version

### Prérequis
```bash
node --version  # v18+ recommandé
npm --version   # v9+ recommandé
```

### Étapes d'Installation

1. **Installer les dépendances**
   ```bash
   cd bonus-linter
   npm install
   ```

2. **Compiler le TypeScript**
   ```bash
   npm run compile
   ```

3. **Créer le package VSIX**
   ```bash
   npm run package
   ```
   
   Cela créera le fichier `ratatouille-language-support-2.0.0.vsix`

4. **Installer l'extension**
   
   Option A - Depuis VS Code :
   - Ouvrir VS Code
   - Extensions (Ctrl+Shift+X)
   - Menu "..." → "Install from VSIX..."
   - Sélectionner le fichier `.vsix`

   Option B - En ligne de commande :
   ```bash
   code --install-extension ratatouille-language-support-2.0.0.vsix
   ```

5. **Recharger VS Code**
   - Ctrl+Shift+P → "Reload Window"
   - Ou redémarrer VS Code

## 🔄 Migration depuis v1.x

### Changements de Configuration

La configuration reste compatible, mais vous avez maintenant accès à :
- Support LSP intégré (activé par défaut)
- Pas besoin de configuration supplémentaire

### Que faire si vous avez modifié l'ancienne version ?

1. Sauvegardez vos modifications personnalisées
2. La nouvelle version est un sur-ensemble de l'ancienne
3. Les modifications de `tmLanguage.json` devraient être compatibles

## 🧪 Tester l'Extension

1. **Ouvrir un fichier .rat**
   ```bash
   code examples/basics/counter.rat
   ```

2. **Tester le Hover**
   - Survolez `Counter` dans `spawn Counter(10)`
   - Vous devriez voir la documentation du proc

3. **Tester l'Autocomplétion**
   - Tapez `pro` et Ctrl+Space
   - Vous devriez voir `proc`, `print`, etc.

4. **Tester Go to Definition**
   - F12 sur un appel de fonction
   - Devrait naviguer vers sa définition

5. **Tester les Snippets**
   - Tapez `proc` et Tab
   - Devrait générer un template complet

## 🛠️ Développement

### Mode Watch
Pour développer en continu :
```bash
npm run watch
```

### Debug
1. Ouvrir le dossier `bonus-linter` dans VS Code
2. F5 pour lancer l'Extension Development Host
3. Ouvrir un fichier .rat dans la nouvelle fenêtre
4. Les breakpoints dans le code TypeScript seront actifs

### Structure du Projet
```
bonus-linter/
├── src/
│   ├── extension.ts     # Client LSP
│   ├── server.ts        # Serveur LSP
│   └── analyzer.ts      # Analyse de documents
├── out/                 # Fichiers compilés (généré)
├── syntaxes/            # Grammaire TextMate
├── snippets/            # Snippets de code
├── package.json         # Manifest
└── tsconfig.json        # Config TypeScript
```

## 🐛 Dépannage

### L'extension ne s'active pas
- Vérifier que le fichier a l'extension `.rat`
- Vérifier dans Output → Ratatouille Language Server

### Pas d'autocomplétion
- Vérifier que le serveur LSP est actif
- Essayer de recharger la fenêtre (Ctrl+Shift+P → Reload Window)

### Erreurs de compilation
```bash
rm -rf node_modules out
npm install
npm run compile
```

### Le hover ne fonctionne pas
- Vérifier que le fichier est bien parsé (pas d'erreurs de syntaxe)
- L'analyseur est basique et peut ne pas reconnaître certaines syntaxes complexes

## 📝 Fonctionnalités Futures (Roadmap)

- [ ] Diagnostics en temps réel (erreurs de syntaxe)
- [ ] Intégration avec le compilateur Glados
- [ ] Rename refactoring
- [ ] Find all references
- [ ] Document symbols (outline)
- [ ] Code actions (quick fixes)
- [ ] Formatting
- [ ] Semantic highlighting
- [ ] Debugger integration

## 🤝 Contribution

Pour contribuer :
1. Fork le repository
2. Créer une branche feature
3. Coder et tester
4. Ouvrir une Pull Request

## 📧 Support

- Issues : https://github.com/vincbct34/Glados-On-Top/issues
- Documentation : https://github.com/vincbct34/Glados-On-Top/tree/main/docs

---

**Bonne mise à jour ! 🚀**
