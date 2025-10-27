# GLaDOS

## 🎯 Présentation (Grand Public)

**GLaDOS** (Generic Language and Data Operand Syntax) est un projet pédagogique dont l’objectif est de **concevoir et développer un langage de programmation complet** en Haskell.

Le projet se déroule en plusieurs étapes :

1. **Un interpréteur LISP minimaliste** – basé sur les S-expressions, proche de Scheme, permettant d’exécuter des programmes simples.
2. **L’évolution vers un langage plus riche** – ajout d’une syntaxe propre (au-delà des parenthèses du LISP), d’une grammaire complète et de nouvelles fonctionnalités.
3. **Un compilateur et une machine virtuelle (VM)** – pour transformer les programmes du langage en bytecode et les exécuter.
4. **Des fonctionnalités avancées** – sécurité, documentation, tests, et éventuellement des bonus comme de nouveaux types (listes, chaînes, flottants), FFI, ou optimisation (TCO).

💡 L’idée n’est pas seulement d’écrire un langage « qui marche », mais aussi de :

* Comprendre les **bases des langages fonctionnels**.
* Explorer la **construction d’un interpréteur, d’un compilateur et d’une VM**.
* Mettre en place une **documentation et un système de tests** digne d’un vrai projet logiciel.

En résumé, **GLaDOS est un projet d’apprentissage qui allie théorie et pratique, en recréant un langage de programmation de zéro**.

---

## ⚙️ Guide Développeur

### 📦 Prérequis

* **Haskell** (Stack)
* **Chez-Scheme** (utile pour comparer le comportement avec la référence Scheme)
* GNU **Make**

### 🛠️ Compilation

Le projet se compile via un **Makefile** avec les règles classiques :

* `make` → compile le projet
* `make re` → recompile à partir de zéro
* `make clean` → supprime les fichiers objets
* `make fclean` → supprime aussi les binaires

Le binaire principal se nomme :

```
glados
```

<!-- ### 🚀 Utilisation

L’exécutable lit un programme depuis **l’entrée standard** ou depuis un fichier :

```bash
# Exemple avec un fichier Scheme
$> cat factorial.scm
(define (fact x)
  (if (eq? x 1)
      1
      (* x (fact (- x 1)))))
(fact 10)

$> ./glados < factorial.scm
3628800
```

En cas d’erreur, le programme s’arrête et retourne **code 84**. -->

### 🔬 Tests & CI/CD

* Les tests unitaires et d’intégration sont obligatoires (framework libre au choix).
* La couverture de code doit être démontrée.
* Une intégration continue (CI) exécute les tests automatiquement à chaque commit.
* Une livraison continue (CD) génère un binaire fonctionnel prêt à l’emploi.

### 📖 Documentation

The project includes comprehensive documentation. See **[Documentation Index](docs/INDEX.md)** for complete listing.

**Quick Links:**
* **[Quick Start Guide](docs/QUICK_START.md)** – Get started in 5 minutes
* **[Language Reference](docs/LANGUAGE_REFERENCE.md)** – Complete syntax guide
* **[Syntax Cheat Sheet](docs/SYNTAX_CHEAT_SHEET.md)** – Quick reference card
* **[Developer Guide](docs/DEVELOPER_GUIDE.md)** – Contributing to the project

**Feature Guides:**
* [Type System](docs/TYPE_SYSTEM_GUIDE.md) · [Arrays & Tuples](docs/ARRAYS_AND_TUPLES_GUIDE.md) · [Constants](docs/CONST_FEATURE_GUIDE.md) · [Booleans](docs/BOOLEAN_IMPLEMENTATION.md) · [Void Type](docs/VOID_TYPE_GUIDE.md)

**Technical:**
* [Grammar (BNF)](docs/nexus.bnf) · [Concurrency Model](docs/NEXUS_CONCEPT.md) · [Project Strategy](docs/PROJECT_STRATEGY.md)

### 📚 Example Programs

The `examples/` directory contains sample programs demonstrating various language features:

**Basic Examples:**
* `examples/basics/helloWorld.rat` – Hello World program
* `examples/basics/counter.rat` – Simple counter with procedures

**Advanced Examples:**
* `examples/advanced/recursiveCounter.rat` – Recursive functions
* `examples/advanced/asynchroneCalc.rat` – Asynchronous calculations
* `examples/advanced/errorHandling.rat` – Error handling patterns
* `examples/advanced/triangularComm.rat` – Process communication
* `examples/advanced/arrayFeatures.rat` – Array operations
* `examples/advanced/voidType.rat` – Void return types
* `examples/advanced/voidParams.rat` – Void parameters
* `examples/advanced/booleans.rat` – Boolean operations
* `examples/advanced/floats.rat` – Float and double handling

### 📂 Organisation

* `src/` → code source principal
* `bonus/` → bonus éventuels (Makefile spécifique inclus)
* `tests/` → jeux de tests unitaires et d’intégration
* `docs/` → documentation et grammaire
