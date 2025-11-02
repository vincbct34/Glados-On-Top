# Changelog - Extension Ratatouille Language Support

Toutes les modifications notables de ce projet seront documentées dans ce fichier.

## [2.0.0] - 2024-11-01

### ✨ Nouvelles Fonctionnalités Majeures

#### Language Server Protocol (LSP)
- **Serveur de langage complet** avec analyse en temps réel
- Architecture modulaire : extension.ts, server.ts, analyzer.ts
- Support de TextDocument synchronization

#### Hover Information 🔍
- Documentation contextuelle au survol des symboles
- Affichage des signatures de proc et func
- Informations de type pour les variables
- Documentation des mots-clés intégrée

#### Go to Definition 🎯
- Navigation vers les définitions de `proc`
- Navigation vers les définitions de `func`
- Navigation vers les déclarations de variables et paramètres
- Support F12 et Ctrl+Click

#### IntelliSense & Auto-completion 💡
- Suggestions intelligentes basées sur le contexte
- Complétion pour procs, funcs, variables
- Suggestions d'atoms du fichier courant
- Complétion des mots-clés et types
- Plus de 200+ suggestions

#### Snippets de Code ⚡
- **30+ snippets** pour toutes les constructions
- Templates pour proc, func, receive, match, if/then/else
- Patterns courants : counter, calculator
- Snippets d'import (all, selected, single)
- Constructions Maybe/Either (Just, None, Left, Right)

### 🎨 Améliorations de Syntaxe

- Support du mot-clé `func` (fonctions pures)
- Tous les mots-clés : if, then, else, match, import, from, scast, rcast
- Types numériques : i8, i16, i32, i64, u8, u16, u32, u64, f32, f64
- Types avancés : Maybe, Either, Array, Tuple, Pid, Bool
- Opérateurs monadiques : >>=
- Commentaires // et # supportés
- Meilleure reconnaissance des patterns
- Distinction proc vs func dans le highlighting
- Support des appels de fonction (entity.name.function.call)

### 🔧 Configuration du Langage

- Auto-closing intelligent des paires
- Indentation contextuelle améliorée
- Règles onEnter personnalisées pour proc, receive, patterns
- Word pattern optimisé pour le langage
- Support du folding avec //region

### 📦 Dépendances et Build

- Mise à jour vers vscode-languageclient ^9.0.1
- Mise à jour vers vscode-languageserver ^9.0.1
- TypeScript 5.3.3
- ESLint et configuration TypeScript moderne
- Scripts de build optimisés

### 📝 Documentation

- README.md complet avec exemples
- UPGRADE_GUIDE.md pour la migration
- Tableau des snippets
- Guide d'installation détaillé
- Architecture du projet documentée

### 🔄 Changements Techniques

- Passage à Node16 module resolution
- Support ES2022
- Configuration TypeScript stricte
- .vscodeignore optimisé pour le packaging
- ESLint avec règles TypeScript

## [1.0.1] - 2025-10-20

### 🔧 Améliorations

- Suppression du thème d'icônes personnalisé (redondant avec Seti)
- Simplification de l'installation via le VS Code Marketplace
- Mise à jour du README avec instructions d'installation marketplace
- Ajout des liens repository dans package.json

### 📝 Documentation

- Instructions claires pour utiliser le thème d'icônes Seti
- Ajout de la commande d'installation marketplace
- Documentation du processus de mise à jour

## [1.0.0] - 2025-10-20

### ✨ Publication Initiale

**🎉 Première publication sur le VS Code Marketplace officiel !**

#### Coloration Syntaxique
- ✅ Support complet de la syntaxe Ratatouille (.rat)
- ✅ Mise en évidence des mots-clés : `proc`, `receive`, `spawn`, `state`, `let`, `self`
- ✅ Coloration des atomes (`:increment`, `:get`, etc.)
- ✅ Support des chaînes de caractères et nombres
- ✅ Reconnaissance des opérateurs spéciaux (`<-`, `->`)
- ✅ Support des commentaires avec `#`

#### Linter Intelligent
- ✅ **ProcSyntaxRule** : Validation des définitions de processus
  - Détection des noms de processus invalides
  - Convention de nommage (avertissement si minuscule initiale)
  
- ✅ **ReceiveSyntaxRule** : Vérification des blocs `receive`
  - Validation de la structure des blocs receive
  - Vérification des patterns avec `|`
  
- ✅ **BracketMatchingRule** : Correspondance des parenthèses/accolades
  - Détection des parenthèses non fermées
  - Détection des accolades mal assorties
  - Ignore les brackets dans les strings et commentaires
  
- ✅ **AtomSyntaxRule** : Validation des atomes
  - Vérification des identifiants après `:`
  - Détection des atomes mal formés
  
- ✅ **MessageSendRule** : Vérification de l'opérateur d'envoi
  - Validation que `<-` a une cible valide
  
- ✅ **StateAssignmentRule** : Gestion d'état
  - Avertissement si `state =` est utilisé sans déclaration initiale
  
- ✅ **UnusedVariableRule** : Variables non utilisées
  - Détection des variables `let` qui ne sont jamais utilisées
  
- ✅ **IndentationRule** : Cohérence de l'indentation
  - Détection du mélange tabs/espaces
  - Recommandations pour une indentation cohérente

#### Autocomplétion
- ✅ Suggestions automatiques pour les mots-clés Ratatouille
- ✅ Complétion des atomes communs (`:get`, `:set`, `:increment`, etc.)
- ✅ Détection et suggestion des processus définis dans le fichier
- ✅ Trigger automatique sur `:` pour les atomes

#### Configuration
- ✅ `ratatouille.linter.enabled` : Activer/désactiver le linter
- ✅ `ratatouille.linter.maxProblems` : Limiter le nombre de problèmes affichés
- ✅ `ratatouille.linter.gladosPath` : Chemin vers le compilateur Glados (optionnel)

#### Intégration VS Code
- ✅ Auto-bracketing pour `{}`, `[]`, `()`, `""`
- ✅ Support du folding (pliage de code)
- ✅ Règles d'indentation automatiques
- ✅ Configuration du langage pour .rat files

### 📦 Infrastructure
- ✅ Build système avec TypeScript
- ✅ Configuration ESLint
- ✅ Configuration de débogage VS Code
- ✅ Scripts npm pour compilation et packaging
- ✅ Documentation complète (README, INSTALL, DEMO)

### 🎯 Tests
- ✅ Fichiers de test inclus (`test.rat`)
- ✅ Fichier de démonstration des fonctionnalités
- ✅ Configuration de lancement pour le débogage

## [Futur]

### Améliorations Potentielles
- [ ] Intégration directe avec le compilateur Glados
- [ ] Support du Language Server Protocol (LSP)
- [ ] Définition de type hover (afficher les signatures)
- [ ] Go to definition pour les processus
- [ ] Refactoring automatique
- [ ] Snippets de code pour les patterns courants
- [ ] Tests unitaires pour les règles du linter
- [ ] Support du debugging interactif
- [ ] Formateur de code (formatter)
- [ ] Documentation inline (hover)

---

**Note:** Cette extension est développée dans le cadre du projet Glados (Epitech) comme bonus pour démontrer les capacités du langage Ratatouille.
