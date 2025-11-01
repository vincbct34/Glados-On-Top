# Ratatouille Language Support for VS Code

Extension complète pour le langage **Ratatouille** (.rat), un langage de programmation basé sur le modèle d'acteurs développé dans le cadre du projet Glados.

## ✨ Fonctionnalités

### 🎨 Coloration Syntaxique Avancée
- Support complet des mots-clés : `proc`, `func`, `receive`, `spawn`, `state`, `let`, `const`, `if`, `then`, `else`, `match`, `import`, etc.
- Reconnaissance des types numériques : `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`
- Types avancés : `Maybe`, `Either`, `Array`, `Tuple`, `Pid`
- Atoms (`:nom_atom`)
- Commentaires `//` et `#`

### 💡 IntelliSense & Autocomplétion
- Suggestions intelligentes pour :
  - Définitions de `proc` et `func`
  - Variables et paramètres
  - Mots-clés du langage
  - Types de données
  - Atoms déjà utilisés dans le fichier

### 🔍 Hover Information
Survolez n'importe quel élément pour voir sa documentation :
- **Procs** : Signature et description des processus
- **Fonctions** : Signature et type de retour
- **Variables** : Type et documentation
- **Mots-clés** : Explication de la syntaxe

### 🎯 Go to Definition
Naviguez instantanément vers la définition de :
- Processus (`proc`)
- Fonctions (`func`)
- Variables et paramètres

### ⚡ Snippets
Plus de 30 snippets pour accélérer votre développement :

| Prefix | Description |
|--------|-------------|
| `proc` | Définition complète de processus avec state |
| `func` | Définition de fonction pure |
| `main` | Point d'entrée principal |
| `receive` | Bloc de réception de messages |
| `if` | Expression conditionnelle if-then-else |
| `match` | Expression de pattern matching |
| `spawn` | Création d'un nouveau processus |
| `send` | Envoi de message |
| `counter` | Pattern complet d'un compteur |
| `import-*` | Différents types d'imports |
| Et bien plus... |

### 🔧 Configuration Automatique
- Auto-fermeture des accolades, parenthèses et crochets
- Indentation intelligente
- Word pattern optimisé pour le langage

## 📦 Installation

### Depuis VSIX
```bash
cd bonus-linter
npm install
npm run compile
npm run package
code --install-extension ratatouille-language-support-2.0.0.vsix
```

### Développement
```bash
cd bonus-linter
npm install
npm run compile
# Puis F5 dans VS Code pour lancer en mode debug
```

## 🚀 Utilisation

1. Ouvrez un fichier `.rat`
2. L'extension s'active automatiquement
3. Profitez de l'IntelliSense, hover, et navigation !

### Exemples de Code

#### Processus Simple
```ratatouille
proc Greeter() {
    receive {
        | (:hello, sender) -> print("Hello!")
        | (:goodbye, sender) -> print("Goodbye!")
    }
}
```

#### Compteur avec État
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

#### Fonction Pure
```ratatouille
func factorial(n) {
    if n <= 1 then
        1
    else
        n * factorial(n - 1)
}
```

## ⚙️ Configuration

Accédez aux paramètres via `Préférences > Paramètres > Extensions > Ratatouille`

- `ratatouille.linter.enabled` : Activer/désactiver le linter (défaut: `true`)
- `ratatouille.linter.gladosPath` : Chemin vers l'exécutable Glados (optionnel)
- `ratatouille.linter.maxProblems` : Nombre maximum de problèmes à reporter (défaut: `100`)

## 🏗️ Architecture

```
bonus-linter/
├── src/
│   ├── extension.ts     # Point d'entrée de l'extension
│   ├── server.ts        # Language Server Protocol
│   └── analyzer.ts      # Analyse de documents .rat
├── syntaxes/
│   └── ratatouille.tmLanguage.json  # Grammaire TextMate
├── snippets/
│   └── ratatouille.json # Snippets de code
├── language-configuration.json  # Configuration du langage
└── package.json         # Manifest de l'extension
```

## 📝 Syntaxe Ratatouille

### Mots-clés Principaux
- `proc` : Définir un processus (acteur)
- `func` : Définir une fonction pure
- `receive` : Bloc de réception de messages
- `spawn` : Créer un nouveau processus
- `state` : État interne d'un processus
- `let` / `const` : Déclaration de variables
- `self` : PID du processus courant
- `if then else` : Expressions conditionnelles
- `match` : Pattern matching
- `import from` : Importer depuis un module

### Types
- Numériques : `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`
- Avancés : `Maybe`, `Either`, `Array`, `Tuple`, `Pid`, `Bool`, `String`

### Opérateurs
- Envoi de message : `<-`
- Flèche : `->`
- Bind monadique : `>>=`
- Arithmétiques : `+`, `-`, `*`, `/`, `%`
- Comparaison : `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logiques : `&&`, `||`, `!`

## 🔗 Liens Utiles

- [Repository GitHub](https://github.com/vincbct34/Glados-On-Top)
- [Documentation Ratatouille](https://github.com/vincbct34/Glados-On-Top/tree/main/docs)

## 🐛 Signaler un Bug

Ouvrez une issue sur [GitHub Issues](https://github.com/vincbct34/Glados-On-Top/issues)

## 📜 Licence

Voir [LICENSE](../LICENSE)

## 👥 Auteurs

Développé dans le cadre du projet Glados - EPITECH 2025

---

**Enjoy coding in Ratatouille! 🐀🍳**
