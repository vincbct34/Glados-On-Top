# Glados-On-Top

[![CI/CD](https://github.com/vincbct34/Glados-On-Top/actions/workflows/CI-CD.yml/badge.svg)](https://github.com/vincbct34/Glados-On-Top/actions/workflows/CI-CD.yml)
[![codecov](https://codecov.io/gh/vincbct34/Glados-On-Top/branch/main/graph/badge.svg)](https://codecov.io/gh/vincbct34/Glados-On-Top)

Un projet Haskell développé avec Stack, avec une pipeline CI/CD complète.

## 🚀 Fonctionnalités

- **Build multi-plateforme** : Compilation sur Ubuntu, Windows et macOS
- **Tests automatisés** : Tests unitaires avec couverture de code
- **Qualité de code** : Linting avec HLint et vérifications de formatage
- **Audit de sécurité** : Vérification des vulnérabilités avec `stack audit`
- **Artefacts** : Génération d'exécutables pour distribution

## 🛠️ Développement

### Prérequis

- [Stack](https://docs.haskellstack.org/en/stable/README/) (gestionnaire de projets Haskell)
- GHC 9.4+ (installé automatiquement par Stack)
- Make (pour utiliser le Makefile)

### Build et exécution

```bash
# Compilation complète
make build

# Compilation rapide (développement)
make fast-build

# Installation des dépendances uniquement
make dependencies

# Installation de l'exécutable dans ./dist
make install

# Tests
make tests_run

# Tests avec couverture
make coverage

# Nettoyage
make clean
make fclean  # nettoyage complet
```

### Outils de qualité

```bash
# Linting avec HLint
make hlint

# Formatage du code (nécessite Ormolu)
make format

# Vérification du formatage
make format-check

# Pipeline CI complète
make ci-all
```

### Aide

```bash
# Afficher toutes les commandes disponibles
make help
```
