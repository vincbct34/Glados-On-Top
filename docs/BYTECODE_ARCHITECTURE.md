# Architecture du Système de Bytecode Ratatouille

## Table des matières

1. [Vue d'ensemble](#vue-densemble)
2. [Architecture modulaire](#architecture-modulaire)
3. [Format binaire](#format-binaire)
4. [Choix des bibliothèques](#choix-des-bibliothèques)
5. [Pipeline de compilation](#pipeline-de-compilation)
6. [Instructions bytecode](#instructions-bytecode)
7. [Optimisations futures](#optimisations-futures)

---

## Vue d'ensemble

Le système de bytecode de Ratatouille est un compilateur intermédiaire qui transforme l'AST (Abstract Syntax Tree) en instructions bytecode exécutables par la machine virtuelle. Le système est conçu pour être :

- **Modulaire** : Séparation claire entre types, compilation, encodage et décodage
- **Efficace** : Format binaire compact et optimisé
- **Extensible** : Facile d'ajouter de nouvelles instructions
- **Debuggable** : Support d'inspection des fichiers binaires

### Flux de données

```
Source .rat → Parser → AST → Compiler → Bytecode → Encoder → Binaire .rtbc
                                                                    ↓
                                                              Decoder → VM
```

---

## Architecture modulaire

Le système de bytecode est divisé en 4 modules distincts, chacun ayant une responsabilité unique :

### 1. `Bytecode/Types.hs` - Définitions de types

**Responsabilité** : Définir les types de données fondamentaux

```haskell
-- Types de valeurs runtime
data Value
  = VInt Integer
  | VFloat Double
  | VString Text
  | VAtom Text
  | VTuple [Value]
  | VArray [Value]
  | VPid Integer
  | VUnit
  | VNone
  | VBool Bool
  | VJust Value
  | VLeft Value
  | VRight Value

-- Instructions bytecode (110+ opcodes)
data Instruction
  = PUSH_INT Integer
  | PUSH_FLOAT Double
  | ADD | SUB | MUL | DIV
  | DEFINE_PROCESS Text [Text] Bytecode
  | ...

type Bytecode = [Instruction]
```

**Choix de conception** :
- **Séparation Value/Instruction** : Les valeurs représentent l'état runtime, les instructions représentent le code
- **Type alias Bytecode** : Simplicité et clarté (`[Instruction]` est plus explicite qu'une liste générique)
- **Instruction récursive** : `DEFINE_PROCESS` contient du bytecode, permettant la définition de processus imbriqués

### 2. `Bytecode/Compiler.hs` - Compilation AST → Bytecode

**Responsabilité** : Transformer l'AST en séquence d'instructions bytecode

```haskell
compileExpr :: Expr -> Bytecode
compileStmt :: Stmt -> Bytecode
compileProgram :: Program -> Bytecode
compilePattern :: Pattern -> Bytecode
```

**Stratégies de compilation** :

#### Expressions arithmétiques
```haskell
-- AST: 2 + 3 * 4
-- Bytecode: [PUSH_INT 2, PUSH_INT 3, PUSH_INT 4, MUL, ADD]
```
Utilise une évaluation en **pile** (stack-based) : les opérandes sont poussés sur la pile, les opérateurs les consomment.

#### Conditionnelles
```haskell
-- AST: if x > 10 then y else z
-- Bytecode:
-- [LOAD_LOCAL "x", PUSH_INT 10, CMP_GT,
--  JUMP_IF_FALSE 3,  -- Saute le then
--  LOAD_LOCAL "y",
--  JUMP 2,           -- Saute le else
--  LOAD_LOCAL "z"]
```
Utilise des **sauts conditionnels** calculés statiquement.

#### Processus et acteurs
```haskell
-- AST: proc Greeter() { receive { ... } }
-- Bytecode:
-- [DEFINE_PROCESS "Greeter" []
--   [PUSH_UNIT, INIT_STATE,
--    PROCESS_LOOP,
--    WAIT_MESSAGE,
--    MATCH_ATOM "hello" 2,
--    PUSH_STRING "Hello!",
--    JUMP (-3),
--    EXIT_PROCESS]]
```
Les processus sont compilés en **boucles de message** avec pattern matching.

**Choix de conception** :
- **Pas de registres** : Architecture stack-based plus simple que register-based
- **Sauts relatifs** : `JUMP n` saute de `n` instructions (positif = avant, négatif = boucle)
- **Variables locales** : `LOAD_LOCAL` / `STORE_LOCAL` pour scope processus
- **Variables globales** : `LOAD_VAR` / `STORE_VAR` pour état partagé

### 3. `Bytecode/Encoder.hs` - Encodage Binaire

**Responsabilité** : Sérialiser le bytecode en format binaire compact

**Bibliothèque utilisée** : `binary` (version 0.8.9.3)

#### Pourquoi `binary` ?

1. **Standard Haskell** : Fait partie de la plateforme Haskell, stable et éprouvé
2. **Type-safe** : Les opérations de sérialisation sont type-checked à la compilation
3. **Lazy ByteString** : Gestion efficace de la mémoire pour grands fichiers
4. **Monade Put** : Interface élégante et composable
5. **Performance** : Optimisé pour la sérialisation binaire

```haskell
import Data.Binary.Put         -- Pour l'encodage
import qualified Data.ByteString.Lazy as BL

encodeBytecode :: Bytecode -> BL.ByteString
encodeInstruction :: Instruction -> Put
```

**Alternative considérée mais rejetée** :
- `cereal` : Plus strict (ByteString strict vs lazy), moins performant pour gros fichiers
- `store` : Trop bas niveau, nécessite gestion manuelle de la mémoire
- Manuel avec `Builder` : Réinventer la roue, erreurs potentielles

#### Style fonctionnel

Au lieu de :
```haskell
PUSH_INT n -> do
  putWord8 0x01
  putWord8 0x00
  encodeInteger n
```

On utilise l'opérateur `>>` :
```haskell
PUSH_INT n -> putWord8 0x01 >> putWord8 0x00 >> encodeInteger n
```

**Avantages** :
- Plus concis et lisible
- Respecte le style fonctionnel
- Évite les blocs `do` superflus
- Le `>>` est l'opérateur standard pour "puis" en monade

### 4. `Bytecode/Decoder.hs` - Décodage Binaire

**Responsabilité** : Désérialiser le format binaire en bytecode

**Bibliothèque utilisée** : `binary` (monade Get)

```haskell
import Data.Binary.Get         -- Pour le décodage

decodeBytecode :: BL.ByteString -> Either String Bytecode
decodeInstruction :: Get Instruction
```

**Gestion d'erreurs** :
- `runGetOrFail` : Retourne `Either` pour gestion explicite des erreurs
- Messages d'erreur descriptifs pour debugging
- Validation du magic number et de la version

---

## Format binaire

### Structure du fichier .rtbc

```
┌──────────────────────────────────────┐
│ HEADER (8 bytes)                     │
├──────────────────────────────────────┤
│ Magic Number: "RTBC" (4 bytes)      │ ← Identification
│ Version Major: 1 (1 byte)           │ ← Compatibilité
│ Version Minor: 0 (1 byte)           │
│ Instruction Count: uint32 (4 bytes) │ ← Nombre d'instructions
├──────────────────────────────────────┤
│ INSTRUCTIONS (variable)              │
├──────────────────────────────────────┤
│ Opcode 1 (1 byte) + Operands        │
│ Opcode 2 (1 byte) + Operands        │
│ ...                                  │
│ Opcode N (1 byte) + Operands        │
└──────────────────────────────────────┘
```

### Encodage des opcodes

Les opcodes sont organisés par catégorie pour faciliter l'extension :

| Plage       | Catégorie                    | Exemples                           |
|-------------|------------------------------|------------------------------------|
| 0x01-0x0F   | Stack operations             | PUSH_INT, PUSH_STRING, PUSH_TUPLE |
| 0x10-0x1F   | Variable operations          | LOAD_VAR, STORE_LOCAL             |
| 0x20-0x2F   | Process state                | INIT_STATE, GET_STATE             |
| 0x30-0x3F   | Arithmetic & String ops      | ADD, SUB, MUL, DIV, CONCAT        |
| 0x40-0x4F   | Comparison & Logic           | CMP_EQ, CMP_LT, LOGIC_AND         |
| 0x50-0x5F   | Value operations             | PUSH_NONE, PUSH_BOOL, GET_FIELD   |
| 0x60-0x6F   | Actor model                  | DEFINE_PROCESS, SEND, SPAWN       |
| 0x70-0x7F   | Pattern matching             | MATCH_ATOM, MATCH_TUPLE           |
| 0x80-0x8F   | Process control              | PROCESS_LOOP, SELF, EXIT          |
| 0x90-0x9F   | Type casting                 | STATIC_CAST, REINTERPRET_CAST     |
| 0xA0-0xAF   | Control flow                 | JUMP, JUMP_IF_FALSE, CALL         |
| 0xFF        | Special                      | HALT                              |

**Avantages de cette organisation** :
- Extensibilité : 15 slots par catégorie
- Lisibilité : Les opcodes similaires sont regroupés
- Debugging : Facile d'identifier la catégorie en hexadécimal

### Encodage des types

#### Entiers (variable-length encoding)

Format optimisé pour minimiser l'espace :

```
┌─────────────────────────────────────┐
│ Size & Sign byte:                   │
│   0x00        → zéro                │
│   0x01-0x7F   → positif (1-127)     │
│   0x81-0xFF   → négatif (1-127)     │
├─────────────────────────────────────┤
│ Value bytes (little-endian)         │
└─────────────────────────────────────┘
```

Exemples :
- `0` → `[0x00]` (1 byte)
- `42` → `[0x01, 0x2A]` (2 bytes)
- `-5` → `[0x81, 0x05]` (2 bytes)
- `256` → `[0x02, 0x00, 0x01]` (3 bytes)

**Avantage** : Petits nombres fréquents = peu d'espace

#### Texte (length-prefixed UTF-8)

```
┌─────────────────────────────────────┐
│ Length: uint32 (4 bytes)            │
├─────────────────────────────────────┤
│ UTF-8 bytes (N bytes)               │
└─────────────────────────────────────┘
```

**Bibliothèque utilisée** : `text` + `bytestring`
- `Data.Text` : Manipulation Unicode sûre
- `Data.Text.Encoding` : Conversion UTF-8 type-safe
- `Data.ByteString` : Stockage binaire efficace

**Pourquoi UTF-8** :
- Standard web et système
- Compact pour ASCII (1 byte/char)
- Support Unicode complet
- Compatible avec la plupart des outils

---

## Choix des bibliothèques

### Vue d'ensemble

| Bibliothèque | Version | Usage | Justification |
|--------------|---------|-------|---------------|
| `binary` | 0.8.9.3 | Sérialisation | Standard, performant, type-safe |
| `bytestring` | 0.12.2.0 | Données binaires | Efficace en mémoire, lazy I/O |
| `text` | Latest | Manipulation texte | Unicode, sécurité, performance |

### `binary` - Sérialisation

**Avantages** :
- ✅ **Stabilité** : Mature, partie de la plateforme Haskell depuis 2007
- ✅ **Type safety** : Impossible de sérialiser incorrectement
- ✅ **Composabilité** : Monades Put/Get faciles à composer
- ✅ **Performance** : Optimisé pour la vitesse et la mémoire
- ✅ **Documentation** : Excellente documentation et exemples

**API élégante** :
```haskell
-- Encoder
runPut :: Put a -> ByteString

-- Decoder  
runGet :: Get a -> ByteString -> a
runGetOrFail :: Get a -> ByteString -> Either String a
```

**Alternatives et leurs inconvénients** :

| Bibliothèque | Inconvénients |
|--------------|---------------|
| `cereal` | ByteString strict (moins efficace pour gros fichiers) |
| `store` | API bas niveau, complexe, risque de bugs |
| `serialise` | Spécifique CBOR, overhead de format |
| Manuel | Réinventer la roue, bugs potentiels, maintenance |

### `bytestring` - Gestion binaire

**Deux types** :
1. **Strict** (`ByteString`) : Tout en mémoire, rapide pour petits fichiers
2. **Lazy** (`Data.ByteString.Lazy`) : Chunks, efficace pour gros fichiers

**Notre choix : Lazy**
```haskell
import qualified Data.ByteString.Lazy as BL
```

**Justification** :
- Fichiers bytecode peuvent être volumineux (gros programmes)
- Lazy I/O permet de traiter le fichier par chunks
- Pas de pénalité pour petits fichiers
- Compatible avec `binary` (utilise lazy par défaut)

**Gestion mémoire** :
```
┌────────────────────────────────────────┐
│ Lazy ByteString                        │
├────────────────────────────────────────┤
│ [Chunk1] → [Chunk2] → [Chunk3] → []   │
│  (32KB)     (32KB)     (16KB)          │
└────────────────────────────────────────┘
```

### `text` - Unicode et texte

**Pourquoi pas String** :
- `String = [Char]` : Liste chaînée, inefficace
- Pas de support Unicode proper
- Lent pour grandes chaînes

**Avantages de Text** :
- ✅ Représentation interne UTF-16 optimisée
- ✅ API riche (split, replace, regex, etc.)
- ✅ Sécurité Unicode (pas d'invalid code points)
- ✅ Performance : 10-100x plus rapide que String
- ✅ Intégration : Compatible avec tout l'écosystème moderne

**Conversion type-safe** :
```haskell
import qualified Data.Text.Encoding as TE

-- Text → ByteString UTF-8
TE.encodeUtf8 :: Text -> ByteString

-- ByteString UTF-8 → Text
TE.decodeUtf8 :: ByteString -> Text
```

---

## Pipeline de compilation

### Étape par étape

#### 1. Parsing (externe à Bytecode)

```
Source: "let x = 42; x + 1"
   ↓
AST: Program [DStmt (SLet "x" Nothing (ELiteral (LInt 42))),
              DStmt (SExpr (EBinOp Add (EVar "x") (ELiteral (LInt 1))))]
```

#### 2. Compilation (Compiler.hs)

```haskell
compileProgram :: Program -> Bytecode
compileProgram (Program definitions) = 
  concatMap compileDefinition definitions ++ [HALT]
```

**Transformations** :
- Variables → `LOAD_LOCAL` / `STORE_LOCAL`
- Opérations → Stack operations
- Contrôle de flux → Jumps
- Processus → Process bytecode

#### 3. Encodage (Encoder.hs)

```haskell
encodeBytecode :: Bytecode -> BL.ByteString
encodeBytecode instructions = runPut $ do
  -- Header
  putLazyByteString magicNumber
  putWord8 versionMajor
  putWord8 versionMinor
  putWord32le (fromIntegral $ length instructions)
  -- Instructions
  mapM_ encodeInstruction instructions
```

**Sortie** :
```
examples/basics/SimpleGreeter.rtbc (format binaire)
```

#### 4. Écriture sur disque

```haskell
writeBinaryFile :: FilePath -> Bytecode -> IO ()
writeBinaryFile filepath bytecode = 
  BL.writeFile filepath (encodeBytecode bytecode)
```

**Avantage ByteString.Lazy** : Écriture streaming, pas de buffer complet en mémoire

---

## Instructions bytecode

### Catégories complètes

#### Stack Operations (0x01-0x0F)

| Opcode | Instruction | Opérandes | Description |
|--------|-------------|-----------|-------------|
| 0x01 | PUSH_INT | type(1) + int(var) | Push integer sur pile |
| 0x01 | PUSH_FLOAT | type(1) + float(8) | Push float sur pile |
| 0x01 | PUSH_STRING | type(1) + text | Push string sur pile |
| 0x01 | PUSH_ATOM | type(1) + text | Push atom sur pile |
| 0x02 | PUSH_TUPLE | size(4) | Pop N éléments, push tuple |
| 0x03 | PUSH_ARRAY | size(4) | Pop N éléments, push array |
| 0x04 | PUSH_UNIT | - | Push unit value |

**Note** : PUSH_* type 0x01 a un sous-type pour distinguer int/float/string/atom

#### Variable Operations (0x10-0x1F)

| Opcode | Instruction | Opérandes | Description |
|--------|-------------|-----------|-------------|
| 0x10 | LOAD_VAR | name | Load global variable |
| 0x11 | STORE_VAR | name | Store global variable |
| 0x12 | LOAD_LOCAL | name | Load local variable (process) |
| 0x13 | STORE_LOCAL | name | Store local variable (process) |
| 0x14 | INDEX | - | Array/tuple indexing |
| 0x15 | ARRAY_LENGTH | - | Get array length |

#### Process State (0x20-0x2F)

| Opcode | Instruction | Description |
|--------|-------------|-------------|
| 0x20 | INIT_STATE | Initialize process state |
| 0x21 | GET_STATE | Push process state |
| 0x22 | SET_STATE | Update process state |

#### Arithmetic (0x30-0x3F)

| Opcode | Instruction | Stack | Description |
|--------|-------------|-------|-------------|
| 0x30 | ADD | [a, b] → [a+b] | Addition |
| 0x31 | SUB | [a, b] → [a-b] | Soustraction |
| 0x32 | MUL | [a, b] → [a*b] | Multiplication |
| 0x33 | DIV | [a, b] → [a/b] | Division |
| 0x34 | CONCAT | [s1, s2] → [s1++s2] | Concaténation string |
| 0x35 | INC_VAR | name | Pre-increment (++x) |
| 0x36 | DEC_VAR | name | Pre-decrement (--x) |
| 0x37 | INC_VAR_POST | name | Post-increment (x++) |
| 0x38 | DEC_VAR_POST | name | Post-decrement (x--) |

**Support polymorphique** : ADD/SUB/MUL/DIV fonctionnent sur Int et Float

#### Actor Model (0x60-0x6F)

| Opcode | Instruction | Opérandes | Description |
|--------|-------------|-----------|-------------|
| 0x60 | DEFINE_PROCESS | name, params[], body[] | Définir processus |
| 0x61 | CREATE_INSTANCE | name | Spawn processus → PID |
| 0x62 | SEND | - | Envoyer message |
| 0x63 | WAIT_MESSAGE | - | Attendre message |

**Exemple complet** :
```haskell
-- Source
proc Counter() {
  state: 0,
  receive {
    | increment -> state + 1
  }
}

-- Bytecode
[DEFINE_PROCESS "Counter" []
  [PUSH_INT 0, INIT_STATE,
   PROCESS_LOOP,
   WAIT_MESSAGE,
   MATCH_ATOM "increment" 2,
   GET_STATE, PUSH_INT 1, ADD, SET_STATE,
   JUMP (-7),
   EXIT_PROCESS],
 HALT]
```

---

## Optimisations futures

### Optimisations prévues

#### 1. Constant folding
```haskell
-- Avant: 2 + 3 * 4
[PUSH_INT 2, PUSH_INT 3, PUSH_INT 4, MUL, ADD]

-- Après: 14
[PUSH_INT 14]
```

#### 2. Dead code elimination
```haskell
-- Avant: if true then x else y
[PUSH_BOOL True, JUMP_IF_FALSE 3, LOAD_VAR "x", JUMP 2, LOAD_VAR "y"]

-- Après:
[LOAD_VAR "x"]
```

#### 3. Tail call optimization
```haskell
-- Avant: récursion → CALL + RETURN
-- Après: récursion terminale → JUMP (pas de stack frame)
```

#### 4. Inline simple functions
```haskell
-- Avant: inc(x) = x + 1; inc(42)
[CALL "inc"]

-- Après:
[PUSH_INT 1, ADD]
```

#### 5. Peephole optimizations
```haskell
-- Pattern: PUSH X, POP
-- Remplacement: []

-- Pattern: JUMP +0
-- Remplacement: []
```

### Extensions possibles

#### 1. Compression du bytecode
- Utiliser `zlib` ou `lz4` pour compresser le fichier .rtbc
- Tradeoff: Taille vs temps de décompression

#### 2. Section de constantes
```
┌──────────────────────────┐
│ HEADER                   │
├──────────────────────────┤
│ CONSTANT POOL            │ ← Strings, nombres constants
├──────────────────────────┤
│ INSTRUCTIONS             │ ← Références vers constant pool
└──────────────────────────┘
```

Avantage : Déduplication des constantes

#### 3. Métadonnées de debug
- Mapping ligne source ↔ bytecode
- Noms de variables originaux
- Stack traces lisibles

#### 4. Profilage intégré
- Compteurs d'exécution par instruction
- Hot paths identification
- Optimisation guidée par profile

---

## Conclusion

L'architecture du système de bytecode Ratatouille est :

✅ **Modulaire** : 4 modules, 1 responsabilité chacun
✅ **Efficace** : Format binaire compact, lazy I/O
✅ **Type-safe** : Haskell + bibliothèques typed
✅ **Fonctionnel** : Style immutable, pas de `do` superflu
✅ **Extensible** : Opcodes organisés, facile d'ajouter des features
✅ **Debuggable** : Inspection des binaires, validation

### Comparaison avec d'autres VMs

| Caractéristique | Ratatouille | Python | JVM | Erlang BEAM |
|-----------------|-------------|--------|-----|-------------|
| Architecture | Stack-based | Stack-based | Stack-based | Register-based |
| Format binaire | Custom .rtbc | .pyc (marshal) | .class | .beam |
| Actor model | ✅ Natif | ❌ (lib) | ❌ (lib) | ✅ Natif |
| Pattern matching | ✅ Bytecode | ❌ (if/else) | ❌ (if/else) | ✅ Bytecode |
| Typed | Graduel | Dynamic | Static | Dynamic |

### Ressources

- [Binary package documentation](https://hackage.haskell.org/package/binary)
- [ByteString performance guide](https://wiki.haskell.org/Performance/ByteString)
- [Text package guide](https://hackage.haskell.org/package/text)
- [VM architecture patterns](https://en.wikipedia.org/wiki/Stack_machine)

---

*Document maintenu par l'équipe Ratatouille - Dernière mise à jour : Octobre 2025*
