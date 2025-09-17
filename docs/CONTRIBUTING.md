# 🤝 Contribuer à GLaDOS

Ce projet a pour objectif de créer un langage de programmation complet en Haskell, en partant d’un interpréteur LISP minimal jusqu’à un compilateur et une machine virtuelle.

Ce document explique **comment contribuer efficacement** au projet.

---

## 📚 Avant de commencer

1. **Lire la documentation** – Familiarisez-vous avec le README et les docs techniques (`docs/`).
2. **Installer les prérequis** :

   * [Haskell / Stack](https://docs.haskellstack.org/en/stable/README/) (recommandé)
   * [Chez-Scheme](https://cisco.github.io/ChezScheme/#get) (pour comparer le comportement du langage avec Scheme)
   * GNU Make
3. **Cloner le dépôt** :

   ```bash
   git clone https://github.com/<username>/glados.git
   cd glados
   ```

---

## 🔧 Compilation & Lancement

* `make` → compile le projet
* `make re` → recompile à partir de zéro
* `make clean` → supprime les fichiers objets
* `make fclean` → supprime binaires + objets

Exécuter un programme :

```bash
./glados < examples/factorial.scm
```

En cas d’erreur, le programme retourne le code **84**.

---

## 📂 Organisation du projet

* `src/` → code source principal
* `tests/` → tests unitaires et d’intégration
* `docs/` → documentation (grammaire, manuel utilisateur, manuel dev)
* `bonus/` → bonus optionnels (Makefile spécifique inclus)
* `examples/` → exemples de programmes en GLaDOS

---

## 🧪 Tests

Les contributions doivent **inclure ou mettre à jour les tests**.

* Framework de tests libre (ex: `hspec`, `tasty`)
* Lancer les tests :

  ```bash
  stack test
  ```
* Vérifiez la **couverture du code** :

  ```bash
  stack test --coverage
  ```

---

## 🚀 Bonnes pratiques

* Respecter la philosophie **fonctionnelle** (pas de variables mutables, pas de fonctions `unsafe`).
* Découper le code en **modules clairs et réutilisables**.
* Respecter le **style Haskell** (indentation, noms explicites, pureté des fonctions).
* Les **messages d’erreur** doivent être explicites et renvoyer le **code 84**.

---

## 📝 Norme des commits et PR

Pour garantir un historique clair et lisible :

### 🔖 Norme des commits

Nous suivons la convention [Conventional Commits](https://www.conventionalcommits.org/) :

```
<type>(scope): message clair et concis
```

Exemples :

* `feat(parser): ajout du support des lambdas`
* `fix(vm): correction de la division par zéro`
* `test(ast): ajout de tests unitaires pour les if`
* `docs: ajout d’exemples dans le README`

Types principaux :

* **feat** → nouvelle fonctionnalité
* **fix** → correction de bug
* **refactor** → modification interne sans changement de comportement
* **docs** → documentation uniquement
* **test** → ajout ou modification de tests
* **chore** → maintenance, CI/CD, dépendances

---

## 🌿 Stratégie de branches

Nous utilisons deux branches longues :

* `main` : branche stable de publication (toujours verte, prête à tag/release)
* `dev` : branche d'intégration (fusion des features avant stabilisation et promotion vers `main`)

Les développements se font sur des branches courtes dérivées de `dev`.

### 🧵 Cycle général

1. Vous créez une branche de travail depuis `dev`.
2. Vous implémentez / testez localement.
3. Vous ouvrez une PR vers `dev`.
4. Après review & merge dans `dev`, d'autres features peuvent s'y ajouter.
5. Quand `dev` est stable (tests verts, validations), on ouvre une PR `dev -> main` pour préparer une release (squash ou merge "no fast-forward").
6. On tag sur `main` (SemVer) après fusion.

### 🔥 Hotfix rapide

En cas de bug critique en production :

1. Créer une branche `hotfix/...` depuis `main`.
2. Corriger, ouvrir PR vers `main`.
3. Après merge, cherry-pick ou rebase le correctif dans `dev` (ou merge `main` dans `dev`).

### 🌱 Branches de travail (feature branches)

Créez toujours une nouvelle branche à partir de `dev` pour chaque modification cohérente :

Préfixes recommandés (en cohérence avec Conventional Commits) :

* `feat/` – nouvelle fonctionnalité
* `fix/` – correction de bug
* `refactor/` – refactorisation sans changement fonctionnel
* `docs/` – documentation uniquement
* `test/` – ajout ou adaptation de tests
* `chore/` – maintenance, CI, dépendances
* `perf/` – amélioration de performance
* `hotfix/` – correctif urgent en production

Format :

```
<prefix>/<courte-description-kebab-case>
```

Exemples :

```
feat/parser-lambdas
fix/vm-division-by-zero
refactor/env-module-simplification
hotfix/lexer-crash-empty-input
```

### 🔁 Mise à jour et synchro

Avant d'ouvrir (ou de finaliser) une PR, rebasez votre branche sur la dernière version de `dev` pour garder un historique linéaire :

```bash
git fetch origin
git rebase origin/dev
```

Si le rebase est complexe ou si plusieurs personnes travaillent sur la même fonctionnalité, un merge peut être accepté, mais le rebase reste préféré.

Push après rebase :

```bash
git push -f origin feat/parser-lambdas
```

Le force push (`-f`) est autorisé uniquement sur vos propres branches de travail (jamais sur `main` ni `dev`).

### 🚨 Branches hotfix

Les branches `hotfix/` partent de `main` et doivent être petites. Une fois mergées, pensez à créer un tag de patch (`vX.Y.(Z+1)`) si pertinent, puis réintégrer le correctif dans `dev`.

### 🧪 Taille et portée

* Une branche = une idée / un groupe de changements cohérents.
* Évitez > ~300 lignes nettes modifiées (sinon découpez).
* Commitez régulièrement (commits atomiques, squash au merge via PR).

### 🪜 Branches empilées (stacked)

Si une fonctionnalité dépend d'un travail encore en review, marquez la seconde PR en *Draft* et mentionnez dans la description :

> Dépend de #<numéro PR>

### 🏷️ Releases & tags

Les versions suivent SemVer : `MAJOR.MINOR.PATCH` (`v1.4.2`).

* Patch (`Z+1`) : corrections de bugs / hotfix
* Minor (`Y+1`) : nouvelle fonctionnalité rétrocompatible
* Major (`X+1`) : changements incompatibles

Création d'un tag (après merge sur `main`) :

```bash
git tag -a v1.2.0 -m "Release v1.2.0"
git push origin v1.2.0
```

### 🧷 Liens avec Issues & PR

Dans la description de la PR, référencez les issues :

```
Fixes #12
Refs #15
```

### ✅ Récap rapide

1. Créer une branche : `git checkout -b feat/ma-fonctionnalite origin/dev`
2. Commits conventionnels : `feat(parser): support des lambdas`
3. Rebase avant PR : `git rebase origin/dev`
4. Ouvrir une PR claire + lier l'issue
5. Merge dans `dev` après review
6. Quand `dev` est prêt : ouvrir PR vers `main`, Squash & Merge (ou merge FF protégé) puis tag (release)

---

### 🔀 Norme des Pull Requests

* Une PR = une fonctionnalité claire (éviter les PR trop grosses).
* Toujours lier la PR à une **issue** si elle existe.
* La description doit contenir :

  * Le but de la PR
  * Les changements principaux
  * Comment tester les modifications
* Les PR doivent être **reviewées et validées** par au moins un autre contributeur avant merge.
* Le merge se fait via **Squash & Merge** pour garder un historique propre.
