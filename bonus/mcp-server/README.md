# Ratatouille MCP Server

Serveur MCP (Model Context Protocol) pour le langage de programmation Ratatouille. Ce serveur fournit un accès structuré à la documentation du langage Ratatouille via le protocole MCP.

## 🚀 Installation

```bash
npm install
```

## 📦 Build

```bash
npm run build
```

## ▶️ Démarrage

```bash
npm start
```

## 📚 Outils MCP disponibles

Le serveur expose plusieurs outils pour interagir avec la documentation et exécuter du code Ratatouille :

### Documentation (générée dynamiquement)

Le serveur télécharge automatiquement la documentation depuis GitHub et génère dynamiquement les outils correspondants. Les outils disponibles dépendent des fichiers markdown présents dans le dossier `docs/` du repository.

#### Outils de documentation générés automatiquement

Pour chaque fichier `.md` trouvé dans `https://github.com/vincbct34/Glados-On-Top/tree/main/docs`, un outil est créé avec le format : `get_ratatouille_<nom_fichier>`

Par exemple :
- `ARCHITECTURE.md` → `get_ratatouille_architecture`
- `CONTRIBUTING.md` → `get_ratatouille_contributing`
- `EXECUTION.md` → `get_ratatouille_execution`

**Paramètres optionnels pour chaque outil :**
- `topic` (string) : Filtrer par un sujet spécifique dans le document

**Exemple :**
```json
{
  "name": "get_ratatouille_architecture",
  "arguments": {
    "topic": "modules"
  }
}
```

#### `get_ratatouille_docs_info`
Liste tous les documents disponibles avec leurs métadonnées (SHA, date de mise à jour, chemin).

**Exemple :**
```json
{
  "name": "get_ratatouille_docs_info",
  "arguments": {}
}
```

#### `search_ratatouille_docs`
Recherche un terme dans toute la documentation disponible.

**Paramètres requis :**
- `query` (string) : Terme à rechercher

**Exemple :**
```json
{
  "name": "search_ratatouille_docs",
  "arguments": {
    "query": "variable"
  }
}
```

### Compilation et Exécution

#### `compile_ratatouille_code`
Compile du code Ratatouille en utilisant le compilateur officiel.

**Paramètres :**
- `code` (string, requis) : Le code source Ratatouille à compiler
- `filename` (string, optionnel) : Nom du fichier source (défaut: "input.gld")

**Exemple :**
```json
{
  "name": "compile_ratatouille_code",
  "arguments": {
    "code": "print(\"Hello World\")",
    "filename": "hello.gld"
  }
}
```

#### `run_ratatouille_code`
Exécute du bytecode Ratatouille avec la machine virtuelle officielle.

**Paramètres :**
- `bytecode` (string, requis) : Le bytecode à exécuter
- `filename` (string, optionnel) : Nom du fichier bytecode (défaut: "program.glb")
- `timeout` (number, optionnel) : Timeout en ms (défaut: 5000)

**Exemple :**
```json
{
  "name": "run_ratatouille_code",
  "arguments": {
    "bytecode": "...",
    "timeout": 10000
  }
}
```

#### `get_binaries_info`
Récupère les informations sur les binaires (compilateur et VM).

**Exemple :**
```json
{
  "name": "get_binaries_info",
  "arguments": {}
}
```

## 🔧 Configuration dans Claude Desktop

Pour utiliser ce serveur MCP avec Claude Desktop, ajoutez la configuration suivante à votre fichier de configuration MCP :

### macOS
Fichier : `~/Library/Application Support/Claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "ratatouille": {
      "command": "node",
      "args": ["/chemin/absolu/vers/glados-bonus-mcp/dist/index.js"]
    }
  }
}
```

### Windows
Fichier : `%APPDATA%\Claude\claude_desktop_config.json`

```json
{
  "mcpServers": {
    "ratatouille": {
      "command": "node",
      "args": ["C:\\chemin\\absolu\\vers\\glados-bonus-mcp\\dist\\index.js"]
    }
  }
}
```

### Linux
Fichier : `~/.config/Claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "ratatouille": {
      "command": "node",
      "args": ["/chemin/absolu/vers/glados-bonus-mcp/dist/index.js"]
    }
  }
}
```

## 🔧 Configuration pour Claude CLI

Pour utiliser ce serveur MCP avec Claude CLI, créez un fichier de configuration dédié et utilisez un alias :

### 1. Créer le fichier de configuration MCP

Créez le fichier `~/.claude/mcp.json` :

```json
{
  "mcpServers": {
    "ratatouille": {
      "command": "node",
      "args": [
        "/chemin/absolu/vers/glados-bonus-mcp/dist/index.js"
      ]
    }
  }
}
```

### 2. Créer un alias pour Claude CLI

Ajoutez cet alias dans votre fichier de configuration shell (`~/.zshrc`, `~/.bashrc`, ou équivalent) :

```bash
alias claude='claude --mcp-config ~/.claude/mcp.json'
```

Puis rechargez votre configuration :

```bash
source ~/.zshrc  # ou source ~/.bashrc
```

### 3. Utilisation

Maintenant, vous pouvez utiliser Claude CLI avec le serveur MCP Ratatouille :

```bash
claude "Comment déclarer une variable en Ratatouille ?"
```

Le serveur MCP sera automatiquement chargé et les outils Ratatouille seront disponibles pour Claude.

## 🔄 Gestion automatique

### Documentation

Le serveur télécharge automatiquement la documentation depuis le repository GitHub :
- **Repository :** https://github.com/vincbct34/Glados-On-Top/tree/main/docs
- **Téléchargement initial :** Au démarrage du serveur
- **Mises à jour automatiques :** Toutes les 120 secondes
- **Génération dynamique :** Les outils MCP sont générés automatiquement pour chaque fichier `.md` trouvé
- **Synchronisation :** Les fichiers supprimés sur GitHub sont automatiquement supprimés localement

La documentation est stockée dans le dossier `docs/` avec un fichier `state.json` qui garde la trace des versions (SHA) de chaque document.

### Binaires

Le serveur télécharge automatiquement les binaires (compilateur et VM) depuis le repository GitHub :
- **Repository :** https://github.com/vincbct34/Glados-On-Top/releases
- **Téléchargement initial :** Au démarrage du serveur
- **Mises à jour automatiques :** Toutes les 120 secondes
- **Détection du système :** Télécharge automatiquement le binaire adapté à votre OS (macOS, Linux, Windows)

Les binaires sont stockés dans le dossier `binaries/` et sont automatiquement rendus exécutables.
