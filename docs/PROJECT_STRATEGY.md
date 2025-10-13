### PROJECT_STRATEGY.md

# Stratégie de Développement pour la Partie 2 : Nexus

## 1\. Introduction

Ce document présente une stratégie pour aborder la Partie 2 du projet GLaDOS en développant le langage "Nexus". Cette approche ne se contente pas de répondre aux exigences ; elle nous permet de construire un projet **original, cohérent et techniquement impressionnant**.

-----

## 2\. Validation des Attendus du Projet ✅

Le modèle de Nexus s'aligne parfaitement avec les quatre axes d'évaluation de la Partie 2.

### Axe 1 : Sécurité et Robustesse (`skill: security`)

C'est le point fort de Nexus. Nous n'avons pas besoin d'ajouter des fonctionnalités de sécurité après coup, elles sont **au cœur du modèle**.

  * **Isolation Mémoire** : Les processus sont des boîtes noires. Pas de pointeurs partagés, pas de *race conditions*. C'est la garantie ultime contre une large classe de bugs.
  * **Tolérance aux Pannes** : Un processus qui plante n'affecte pas les autres. Nous pouvons expliquer ce concept de "Let it crash", une philosophie de conception de systèmes robustes.
  * **API Explicites** : La communication par messages force à définir des interfaces propres pour chaque composant du système.

### Axe 2 : Syntaxe, Grammaire et Sémantique (`skill: parsing`)

Nexus remplit toutes les conditions :

  * **Grammaire non-LISP** : La syntaxe est radicalement différente et conçue pour le modèle d'acteurs.
  * **Cohérence** : La syntaxe (`proc`, `spawn`, `<-`) sert directement la sémantique du langage.
  * **BNF Fournie** : Une grammaire formelle est déjà ébauchée (`nexus.bnf`), validant une exigence obligatoire.

### Axe 3 : Évaluation et Compilation (`skill: evaluation / compilation`)

Le projet nous impose de créer une VM et un compilateur. Nexus rend cette tâche passionnante.

  * **Machine Virtuelle (VM) Avancée** : Notre VM ne sera pas une simple machine à pile. Elle sera un véritable micro-environnement d'exécution qui devra implémenter :
    1.  Un **Scheduler** de processus.
    2.  Une **gestion de la mémoire par processus**.
    3.  Un système de **boîtes aux lettres (mailboxes)**.
  * **Bytecode Spécifique** : Le compilateur générera un jeu d'instructions de haut niveau, parfaitement adapté à notre VM (ex: `SPAWN`, `SEND`, `RECEIVE`). C'est bien plus intéressant qu'une simple traduction d'opérations arithmétiques.
  * **Exigences techniques respectées** : La chaîne complète (parser -\> compilateur -\> bytecode binaire -\> VM) est respectée.

### Axe 4 : Documentation (`skill: documentation`)

Le modèle unique de Nexus nous donne une matière riche pour la documentation.

  * **Manuel Utilisateur** : Nous pourrons expliquer un paradigme de programmation original.
  * **Analyse de Sécurité** : Nous pourrons comparer l'approche de Nexus (isolation) avec celles de langages comme C (dangereuse) ou Python (verrous complexes), ce qui est explicitement demandé.

-----

## 3\. Plan de Développement Suggéré 🗺️

Pour ne pas se perdre, nous pouvons diviser le projet en étapes claires et logiques.

**Étape 1 : Le Parser & l'AST (Base Solide)**

  * Objectif : Traduire le code source `nexus` en Arbre Syntaxique Abstrait (AST) en Haskell.
  * Outils : Utiliser la grammaire `nexus.bnf` comme guide avec la bibliothèque `Megaparsec`.

**Étape 2 : Le Compilateur & le Bytecode (Traduction)**

  * Objectif : Définir notre jeu d'instructions (ex: `PUSH_INT`, `SPAWN`, `SEND`).
  * Tâche : Écrire une fonction qui parcourt l'AST et génère une liste d'instructions (le bytecode).

**Étape 3 : Le Cœur de la VM (Le Moteur)**

  * Objectif : Créer la boucle d'exécution principale de la VM, le scheduler de processus et le système de messages.
  * C'est l'étape la plus complexe, mais aussi la plus gratifiante. On peut commencer avec un scheduler très simple ("round-robin").

**Étape 4 : Intégration & Fonctions de Base**

  * Objectif : Faire fonctionner la chaîne complète. Un programme simple doit pouvoir être compilé et exécuté.
  * Tâche : Implémenter les fonctions natives nécessaires (ex: `print`, la gestion de `self`).

**Étape 5 : Finalisation & Documentation**

  * Objectif : Nettoyer le code, gérer les erreurs proprement, et rédiger la documentation finale en s'appuyant sur les points forts de notre architecture.

En suivant ce plan, nous pouvons construire un projet ambitieux de manière structurée et maîtriser la complexité.