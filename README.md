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

* Manuel utilisateur (syntaxe, exemples).
* Grammaire formelle du langage (BNF conseillée).
* Description du compilateur et de la VM.
* Manuel développeur pour étendre le langage.

### 📂 Organisation

* `src/` → code source principal
* `bonus/` → bonus éventuels (Makefile spécifique inclus)
* `tests/` → jeux de tests unitaires et d’intégration
* `docs/` → documentation et grammaire
