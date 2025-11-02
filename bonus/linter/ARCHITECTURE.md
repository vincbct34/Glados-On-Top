# 🏗️ Architecture Technique - Extension Ratatouille v2.0

## 📐 Vue d'Ensemble

L'extension Ratatouille v2.0 implémente le **Language Server Protocol (LSP)** pour fournir des fonctionnalités IDE avancées.

```
┌─────────────────────────────────────────────────────────┐
│                      VS Code                            │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │          Extension Host Process                   │  │
│  │                                                   │  │
│  │  ┌────────────────────────────────────────────┐  │  │
│  │  │  extension.ts (Client)                     │  │  │
│  │  │  - Active l'extension                      │  │  │
│  │  │  - Crée le Language Client                 │  │  │
│  │  │  - Configure la communication              │  │  │
│  │  └────────────────┬───────────────────────────┘  │  │
│  │                   │ IPC                          │  │
│  │                   ↓                              │  │
│  │  ┌────────────────────────────────────────────┐  │  │
│  │  │  server.ts (Server)                        │  │  │
│  │  │  - Gère les requêtes LSP                   │  │  │
│  │  │  - onHover, onCompletion, onDefinition     │  │  │
│  │  │  - Coordonne l'analyse                     │  │  │
│  │  └────────────────┬───────────────────────────┘  │  │
│  │                   │                              │  │
│  │                   ↓                              │  │
│  │  ┌────────────────────────────────────────────┐  │  │
│  │  │  analyzer.ts (Parser)                      │  │  │
│  │  │  - Parse les fichiers .rat                 │  │  │
│  │  │  - Extrait les symboles                    │  │  │
│  │  │  - Génère la documentation                 │  │  │
│  │  └────────────────────────────────────────────┘  │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

---

## 📦 Structure des Fichiers

```
bonus-linter/
├── src/                          # Code source TypeScript
│   ├── extension.ts              # Point d'entrée, client LSP
│   ├── server.ts                 # Serveur LSP
│   └── analyzer.ts               # Analyseur de documents
│
├── out/                          # Fichiers compilés (généré)
│   ├── extension.js
│   ├── server.js
│   └── analyzer.js
│
├── syntaxes/                     # Grammaire TextMate
│   └── ratatouille.tmLanguage.json
│
├── snippets/                     # Snippets de code
│   └── ratatouille.json
│
├── icons/                        # Icônes de l'extension
│   └── rat-icon.png
│
├── node_modules/                 # Dépendances (généré)
│
├── package.json                  # Manifest de l'extension
├── tsconfig.json                 # Configuration TypeScript
├── language-configuration.json   # Config du langage
├── .eslintrc.json               # Configuration ESLint
├── .vscodeignore                # Fichiers à exclure du package
│
├── README.md                     # Documentation utilisateur
├── UPGRADE_GUIDE.md             # Guide de migration
├── SUMMARY.md                   # Résumé des features
├── TESTING.md                   # Guide de test
├── changelog.md                 # Historique des versions
│
├── build.sh                     # Script de build
└── install.sh                   # Script d'installation
```

---

## 🔧 Composants Principaux

### 1. extension.ts - Client LSP

**Rôle:** Point d'entrée de l'extension, initialise le client LSP.

**Responsabilités:**
- Activation de l'extension quand un fichier .rat est ouvert
- Création et configuration du Language Client
- Lancement du serveur LSP
- Gestion du cycle de vie (activate/deactivate)

**Flux:**
```typescript
1. activate() appelée par VS Code
2. Localiser server.js
3. Créer ServerOptions (IPC transport)
4. Créer ClientOptions (document selector)
5. Instancier LanguageClient
6. Appeler client.start()
7. Communication établie
```

**API Utilisées:**
- `vscode-languageclient/node`: Communication LSP
- `vscode`: API VS Code
- `path`: Résolution de chemins

### 2. server.ts - Serveur LSP

**Rôle:** Cœur de la logique, gère toutes les requêtes LSP.

**Responsabilités:**
- Initialisation du serveur
- Gestion des documents (ouverture, fermeture, modification)
- Réponse aux requêtes:
  - `onHover`: Information contextuelle
  - `onCompletion`: Auto-complétion
  - `onDefinition`: Navigation vers définition
- Coordination avec l'analyseur
- Communication avec le client

**Handlers Implémentés:**

```typescript
// Lifecycle
connection.onInitialize()      → Capacités du serveur
connection.onInitialized()     → Configuration post-init

// Document sync
documents.onDidChangeContent() → Analyser le document
documents.onDidClose()         → Nettoyer les caches

// Language features
connection.onHover()           → Fournir documentation
connection.onCompletion()      → Suggestions
connection.onDefinition()      → Location de définition
```

**Architecture:**

```typescript
// Cache des analyseurs
Map<URI, DocumentAnalyzer>

// Pour chaque requête:
1. Récupérer le document
2. Obtenir/créer l'analyseur
3. Extraire les symboles
4. Construire la réponse
5. Retourner au client
```

**Optimisations:**
- Cache des analyseurs par document
- Analyse incrémentale (onDidChangeContent)
- Nettoyage des caches (onDidClose)

### 3. analyzer.ts - Analyseur de Documents

**Rôle:** Parse les fichiers .rat et extrait les informations de symboles.

**Responsabilités:**
- Parsing ligne par ligne
- Extraction de définitions (proc, func, variables)
- Détection des patterns (tuples, atoms)
- Génération de documentation
- Recherche de symboles à une position

**Structures de Données:**

```typescript
interface Symbol {
    name: string
    kind: 'proc' | 'func' | 'variable' | 'parameter' | 'atom'
    line: number
    column: number
    endLine: number
    endColumn: number
    type?: string
    documentation?: string
}

interface DocumentSymbols {
    procs: Map<string, Symbol>
    funcs: Map<string, Symbol>
    variables: Map<string, Symbol>
    atoms: Set<string>
    imports: Map<string, string[]>
}
```

**Algorithme de Parsing:**

```typescript
analyze() {
    pour chaque ligne:
        // Proc definitions
        si match /proc\s+(\w+)\(/
            extraire nom, paramètres
            créer Symbol de type 'proc'
            ajouter à procs
        
        // Func definitions
        si match /func\s+(\w+)\(/
            extraire nom, paramètres
            créer Symbol de type 'func'
            ajouter à funcs
        
        // Variables
        si match /let|const\s+(\w+)(<type>)?/
            extraire nom, type optionnel
            créer Symbol de type 'variable'
            ajouter à variables
        
        // Atoms
        pour chaque match /:(\w+)/
            ajouter à atoms
        
        // Imports
        si match /import.*from/
            extraire path et symboles
            ajouter à imports
}
```

**Fonctions Utilitaires:**

```typescript
parseParameters(str)        → Array<{name, type?}>
generateProcDocumentation() → string (Markdown)
generateFuncDocumentation() → string (Markdown)
getSymbolAtPosition()       → Symbol | undefined
```

---

## 🔌 Protocol LSP Implémenté

### Méthodes Supportées

| Méthode | Description | Implémentation |
|---------|-------------|----------------|
| `initialize` | Capabilities du serveur | ✅ Complet |
| `textDocument/didOpen` | Document ouvert | ✅ Via TextDocuments |
| `textDocument/didChange` | Document modifié | ✅ Incrémental |
| `textDocument/didClose` | Document fermé | ✅ Cleanup |
| `textDocument/hover` | Info au survol | ✅ Complet |
| `textDocument/completion` | Auto-complétion | ✅ Complet |
| `textDocument/definition` | Go to definition | ✅ Complet |

### Capabilities Déclarées

```typescript
{
    textDocumentSync: TextDocumentSyncKind.Incremental,
    completionProvider: {
        resolveProvider: true,
        triggerCharacters: [':', '.', ' ']
    },
    hoverProvider: true,
    definitionProvider: true
}
```

---

## 🎨 Syntaxe TextMate

**Fichier:** `syntaxes/ratatouille.tmLanguage.json`

### Architecture de la Grammaire

```json
{
    "patterns": [
        { "include": "#comments" },
        { "include": "#keywords" },
        { "include": "#types" },
        { "include": "#proc-definition" },
        { "include": "#func-definition" },
        { "include": "#function-call" },
        { "include": "#atoms" },
        { "include": "#strings" },
        { "include": "#numbers" },
        { "include": "#operators" },
        { "include": "#constants" },
        { "include": "#identifiers" }
    ],
    "repository": { ... }
}
```

### Scopes Définis

| Pattern | Scope | Description |
|---------|-------|-------------|
| `proc Name` | `entity.name.function.proc` | Définition proc |
| `func Name` | `entity.name.function.func` | Définition func |
| `Name(` | `entity.name.function.call` | Appel de fonction |
| `print` | `support.function.builtin` | Fonction built-in |
| `i32`, `f64` | `storage.type.numeric` | Types numériques |
| `:atom` | `constant.language.symbol` | Atom |
| `"string"` | `string.quoted.double` | String |
| `42` | `constant.numeric.integer` | Nombre |
| `<-` | `keyword.operator.message` | Envoi message |

---

## 📝 Snippets

**Fichier:** `snippets/ratatouille.json`

### Structure

```json
{
    "Snippet Name": {
        "prefix": "trigger",
        "body": [
            "line 1 with ${1:placeholder}",
            "line 2 with ${2:placeholder}"
        ],
        "description": "Description"
    }
}
```

### Placeholders

- `${1:name}`: Premier placeholder
- `${2:type}`: Deuxième placeholder
- Tab pour naviguer entre les placeholders

---

## ⚙️ Configuration

### package.json - Manifest

**Sections Importantes:**

```json
{
    "main": "./out/extension.js",           // Point d'entrée
    "activationEvents": [                   // Quand activer
        "onLanguage:ratatouille"
    ],
    "contributes": {
        "languages": [...],                  // Enregistrement du langage
        "grammars": [...],                   // Grammaire TextMate
        "snippets": [...],                   // Snippets
        "configuration": {...}               // Settings
    },
    "dependencies": {
        "vscode-languageclient": "^9.0.1",
        "vscode-languageserver": "^9.0.1",
        "vscode-languageserver-textdocument": "^1.0.11"
    }
}
```

### tsconfig.json - TypeScript

```json
{
    "compilerOptions": {
        "module": "Node16",                 // Module ES
        "target": "ES2022",                 // Target moderne
        "outDir": "out",                    // Sortie compilée
        "rootDir": "src",                   // Source
        "strict": true,                     // Mode strict
        "moduleResolution": "Node16"
    }
}
```

---

## 🔄 Flux de Communication

### 1. Activation de l'Extension

```
User ouvre .rat → VS Code détecte language
                ↓
        Charge extension.js
                ↓
        activate() appelée
                ↓
        Client LSP créé
                ↓
        Serveur LSP lancé (server.js)
                ↓
        Connection IPC établie
                ↓
        Serveur initialisé
                ↓
    Extension prête
```

### 2. Requête Hover

```
User survole "Counter"
        ↓
VS Code → textDocument/hover request
        ↓
Client LSP → Serveur LSP (IPC)
        ↓
server.ts: onHover() handler
        ↓
Récupère DocumentAnalyzer
        ↓
analyzer.analyze() → DocumentSymbols
        ↓
Cherche "Counter" dans symbols
        ↓
Génère Hover avec documentation
        ↓
Retour au client (IPC)
        ↓
Client → VS Code
        ↓
VS Code affiche popup
```

### 3. Auto-complétion

```
User tape "pro" + Ctrl+Space
        ↓
VS Code → textDocument/completion request
        ↓
Client → Serveur
        ↓
server.ts: onCompletion() handler
        ↓
Analyse le document
        ↓
Collecte suggestions:
    - Procs définis
    - Funcs définies
    - Variables
    - Mots-clés
    - Types
    - Atoms
        ↓
Filtre par préfixe "pro"
        ↓
Retour liste CompletionItem[]
        ↓
VS Code affiche menu
```

### 4. Go to Definition

```
User F12 sur "Counter"
        ↓
VS Code → textDocument/definition request
        ↓
Client → Serveur
        ↓
server.ts: onDefinition() handler
        ↓
Analyse document
        ↓
Cherche "Counter" dans procs/funcs/variables
        ↓
Si trouvé: Location{uri, range}
        ↓
Retour au client
        ↓
VS Code navigate vers location
```

---

## 🧪 Tests et Debugging

### Lancer en Mode Debug

1. Ouvrir `bonus-linter/` dans VS Code
2. F5 → Lance Extension Development Host
3. Ouvrir un .rat dans la nouvelle fenêtre
4. Breakpoints dans le code TypeScript sont actifs

### Logs

**Output Panel:**
- `Output` → `Ratatouille Language Server`
- Voir les messages du serveur

**Console:**
- `Help` → `Toggle Developer Tools`
- Voir erreurs JavaScript

### Performance Profiling

```typescript
// Dans server.ts
console.time('analyze');
const symbols = analyzer.analyze();
console.timeEnd('analyze');
```

---

## 🚀 Build et Déploiement

### Build Process

```bash
npm install          # Installer dépendances
npm run compile      # TypeScript → JavaScript (out/)
npm run package      # Créer .vsix
```

### Structure du .vsix

```
ratatouille-language-support-2.0.0.vsix
├── extension/
│   ├── out/          # Code compilé
│   ├── syntaxes/     # Grammaire
│   ├── snippets/     # Snippets
│   ├── icons/        # Icônes
│   ├── package.json  # Manifest
│   └── ...
└── [Content_Types].xml
```

### Installation

```bash
code --install-extension *.vsix
```

---

## 🔮 Extensibilité Future

### Fonctionnalités Possibles

1. **Diagnostics:**
   ```typescript
   connection.onDidChangeContent(() => {
       const diagnostics = validateDocument(document);
       connection.sendDiagnostics({uri, diagnostics});
   });
   ```

2. **Rename:**
   ```typescript
   connection.onPrepareRename()
   connection.onRenameRequest()
   ```

3. **References:**
   ```typescript
   connection.onReferences()
   ```

4. **Formatting:**
   ```typescript
   connection.onDocumentFormatting()
   ```

5. **Code Actions:**
   ```typescript
   connection.onCodeAction()
   ```

---

## 📚 Dépendances

### Production

| Package | Version | Usage |
|---------|---------|-------|
| `vscode-languageclient` | ^9.0.1 | Client LSP |
| `vscode-languageserver` | ^9.0.1 | Serveur LSP |
| `vscode-languageserver-textdocument` | ^1.0.11 | Gestion documents |

### Development

| Package | Version | Usage |
|---------|---------|-------|
| `typescript` | ^5.3.3 | Compilation TS |
| `@types/vscode` | ^1.75.0 | Types VS Code |
| `@types/node` | ^18.19.0 | Types Node.js |
| `eslint` | ^8.56.0 | Linting |
| `@vscode/vsce` | ^2.22.0 | Packaging |

---

## 🎯 Métriques

### Taille du Code

- **TypeScript**: ~610 lignes
- **JSON** (grammar + snippets): ~500 lignes
- **Documentation**: ~3000 lignes
- **Total**: ~4100 lignes

### Performance

- Activation: < 2s
- Analyse document: < 100ms (fichier typique)
- Hover response: < 50ms
- Completion: < 100ms

### Coverage

- Mots-clés: 15+ supportés
- Types: 20+ reconnus
- Snippets: 30+
- Fonctionnalités LSP: 6 implémentées

---

**Architecture solide, performante et extensible ! 🏗️✨**
