# 🧪 Guide de Test - Extension Ratatouille v2.0

Ce document fournit des scénarios de test pour valider toutes les fonctionnalités de l'extension.

## 📋 Checklist de Test

### ✅ Installation et Activation

- [ ] L'extension s'installe sans erreur
- [ ] VS Code détecte les fichiers `.rat`
- [ ] L'icône Ratatouille apparaît pour les fichiers `.rat`
- [ ] Le Language Server démarre (vérifier dans Output → Ratatouille Language Server)

**Test:**
```bash
cd bonus-linter
./install.sh
code ../examples/basics/counter.rat
```

---

## 🎨 1. Coloration Syntaxique

### Test des Mots-clés

**Fichier de test:** `examples/basics/counter.rat`

Vérifier que ces éléments sont correctement colorés:
- [ ] `proc` en violet/keyword
- [ ] `receive` en violet/keyword
- [ ] `state` en violet/keyword
- [ ] `let` en violet/keyword
- [ ] `spawn` en violet/keyword

### Test des Types

**Fichier de test:** `examples/basics/TypedVariables.rat`

- [ ] `i32`, `f64`, `u8` colorés comme types
- [ ] Types entre `<` `>` dans `let age<i32> = 25`

### Test des Opérateurs

- [ ] `<-` (flèche d'envoi de message) distinct
- [ ] `->` (flèche de pattern) distinct
- [ ] `==`, `!=`, `<=`, `>=` reconnus
- [ ] `&&`, `||` reconnus

### Test des Atoms

**Code:**
```ratatouille
:hello
:increment
:get
```

- [ ] Atoms colorés différemment (constant/symbol)
- [ ] `:` attaché au nom de l'atom

### Test des Commentaires

**Code:**
```ratatouille
// Commentaire moderne
# Commentaire legacy
```

- [ ] Les deux styles sont reconnus
- [ ] Grisés/commentaires

---

## 💡 2. IntelliSense & Auto-complétion

### Test 1: Mots-clés

**Action:** Dans un fichier .rat, tapez `pro` puis `Ctrl+Space`

**Attendu:**
- [ ] Suggestions incluent: `proc`, `print`
- [ ] Chaque suggestion a une description

### Test 2: Types

**Action:** Tapez `let x<` puis `Ctrl+Space`

**Attendu:**
- [ ] Suggestions de types: `i32`, `i64`, `f32`, etc.
- [ ] Types avancés: `Array`, `Maybe`, `Either`

### Test 3: Procs Définis

**Code:**
```ratatouille
proc Counter(initial) { ... }
proc Worker() { ... }

proc main() {
    spa  # <- Taper Ctrl+Space ici
}
```

**Attendu:**
- [ ] `spawn` suggéré
- [ ] `Counter` suggéré
- [ ] `Worker` suggéré

### Test 4: Atoms

**Code:**
```ratatouille
proc Test() {
    receive {
        | :hello -> print("hi")
    }
}

proc main() {
    let t = spawn Test()
    t <- :  # <- Ctrl+Space ici
}
```

**Attendu:**
- [ ] `:hello` suggéré dans les completions

---

## 🔍 3. Hover Information

### Test 1: Hover sur Proc

**Code:**
```ratatouille
proc Counter(initial) {
    state: initial,
    receive {
        | :increment -> state = state + 1
    }
}

proc main() {
    let c = spawn Counter(10)  # <- Survoler "Counter"
}
```

**Attendu:**
- [ ] Popup avec documentation
- [ ] Contient "proc Counter(initial)"
- [ ] Description du processus

### Test 2: Hover sur Variable

**Code:**
```ratatouille
proc main() {
    let count<i32> = 0  # <- Survoler "count"
}
```

**Attendu:**
- [ ] Affiche "variable count: i32"

### Test 3: Hover sur Mot-clé

**Code:**
```ratatouille
receive {  # <- Survoler "receive"
    | x -> print(x)
}
```

**Attendu:**
- [ ] Documentation du mot-clé `receive`
- [ ] Explication de la syntaxe

### Test 4: Hover sur Atom

**Code:**
```ratatouille
:increment  # <- Survoler
```

**Attendu:**
- [ ] Description: "Atomic constant value used for pattern matching"

---

## 🎯 4. Go to Definition

### Test 1: Go to Proc

**Code:**
```ratatouille
proc Worker() {  # <- Ligne 1 (définition)
    receive { | x -> print(x) }
}

proc main() {    # <- Ligne 5
    let w = spawn Worker()  # <- Ligne 6
}
```

**Action:** F12 sur `Worker` ligne 6

**Attendu:**
- [ ] Curseur saute à ligne 1
- [ ] `Worker` est sélectionné

### Test 2: Go to Variable

**Code:**
```ratatouille
proc main() {
    let counter = spawn Counter(0)  # <- Ligne 2 (définition)
    
    counter <- :increment  # <- Ligne 4
}
```

**Action:** F12 sur `counter` ligne 4

**Attendu:**
- [ ] Saute à ligne 2
- [ ] `counter` sélectionné

### Test 3: Ctrl+Click

**Action:** Ctrl+Click sur n'importe quelle définition

**Attendu:**
- [ ] Même comportement que F12

---

## ⚡ 5. Snippets

### Test 1: Snippet proc

**Action:** Tapez `proc` puis `Tab`

**Attendu:**
```ratatouille
proc Name(params) {
    state: initial_state,
    receive {
        | pattern -> expression
    }
}
```
- [ ] Template complet généré
- [ ] Curseur sur `Name`
- [ ] Tab permet de naviguer entre les placeholders

### Test 2: Snippet main

**Action:** Tapez `main` puis `Tab`

**Attendu:**
```ratatouille
proc main() {
    // Your code here
}
```

### Test 3: Snippet if

**Action:** Tapez `if` puis `Tab`

**Attendu:**
```ratatouille
if condition then
    then_expr
else
    else_expr
```

### Test 4: Snippet counter

**Action:** Tapez `counter` puis `Tab`

**Attendu:**
- [ ] Template complet d'un compteur fonctionnel
- [ ] Avec state et tous les messages

### Test 5: Liste Complète

Tester que ces snippets fonctionnent:
- [ ] `proc`
- [ ] `proc-simple`
- [ ] `func`
- [ ] `main`
- [ ] `receive`
- [ ] `case`
- [ ] `if`
- [ ] `if-simple`
- [ ] `match`
- [ ] `let`
- [ ] `let-typed`
- [ ] `const`
- [ ] `spawn`
- [ ] `send`
- [ ] `send-tuple`
- [ ] `print`
- [ ] `import-all`
- [ ] `import-selected`
- [ ] `import-single`
- [ ] `counter`
- [ ] `tuple`
- [ ] `array`
- [ ] `index`
- [ ] `scast`
- [ ] `rcast`
- [ ] `just`
- [ ] `none`
- [ ] `left`
- [ ] `right`
- [ ] `atom`
- [ ] `block`
- [ ] `comment`

---

## 🔧 6. Configuration du Langage

### Test 1: Auto-closing

**Action:** Tapez `{`

**Attendu:**
- [ ] `}` automatiquement inséré
- [ ] Curseur entre les deux

**Tester aussi:**
- [ ] `(` → `()`
- [ ] `[` → `[]`
- [ ] `"` → `""`

### Test 2: Indentation

**Code initial:**
```ratatouille
proc Test() {
```

**Action:** Appuyez sur Enter

**Attendu:**
- [ ] Nouvelle ligne indentée automatiquement

**Test avec receive:**
```ratatouille
receive {
```
**Action:** Enter

**Attendu:**
- [ ] Indentation augmentée

### Test 3: Dé-indentation

**Code:**
```ratatouille
proc Test() {
    let x = 1
}  # <- La fermeture se dé-indente automatiquement
```

**Action:** Tapez `}` après une ligne indentée

**Attendu:**
- [ ] `}` se dé-indente au niveau de `proc`

---

## 🔬 7. Tests d'Intégration

### Test 1: Fichier Complet

**Fichier:** `examples/basics/counter.rat`

Valider:
- [ ] Syntaxe complète colorée
- [ ] Hover fonctionne sur tous les symboles
- [ ] Go to definition fonctionne
- [ ] Auto-complétion propose les bons symbols
- [ ] Pas d'erreur dans Output

### Test 2: Fichier avec Imports

**Fichier:** `examples/test/importSelected.rat`

Valider:
- [ ] Import statement reconnu
- [ ] Symboles importés dans auto-complétion

### Test 3: Fichier avec Types

**Fichier:** `examples/basics/TypedVariables.rat`

Valider:
- [ ] Types numériques colorés
- [ ] Types dans hover
- [ ] Types dans auto-complétion

---

## 🐛 8. Tests de Robustesse

### Test 1: Fichier Vide

**Action:** Créer un fichier .rat vide

**Attendu:**
- [ ] Pas de crash
- [ ] Extension active
- [ ] Auto-complétion disponible

### Test 2: Syntaxe Invalide

**Code:**
```ratatouille
proc Test( {
    asdfasdf
```

**Attendu:**
- [ ] Extension ne crash pas
- [ ] Hover et complétion continuent de fonctionner partiellement

### Test 3: Gros Fichier

**Action:** Ouvrir un fichier avec 1000+ lignes

**Attendu:**
- [ ] Performance acceptable
- [ ] Pas de lag
- [ ] Toutes les fonctionnalités opérationnelles

### Test 4: Plusieurs Fichiers

**Action:** Ouvrir 5+ fichiers .rat simultanément

**Attendu:**
- [ ] Chaque fichier analysé indépendamment
- [ ] Pas de confusion entre les symboles
- [ ] Pas de fuite mémoire

---

## 📊 9. Tests de Performance

### Test 1: Temps d'Activation

**Action:** Ouvrir un fichier .rat

**Mesurer:**
- [ ] Extension s'active en < 2 secondes
- [ ] Language Server prêt en < 3 secondes

### Test 2: Réactivité

**Action:** Taper du code rapidement

**Attendu:**
- [ ] Pas de lag visible
- [ ] Syntaxe colorée en temps réel
- [ ] Auto-complétion réactive

### Test 3: Hover Response Time

**Action:** Survoler rapidement plusieurs symboles

**Attendu:**
- [ ] Popup apparaît en < 500ms
- [ ] Pas de freeze

---

## ✅ Validation Finale

### Checklist Globale

Après tous les tests:
- [ ] Aucun crash observé
- [ ] Toutes les fonctionnalités opérationnelles
- [ ] Performance acceptable
- [ ] Expérience utilisateur fluide
- [ ] Pas d'erreur dans Console
- [ ] Pas d'erreur dans Output

### Output Logs

Vérifier dans **Output → Ratatouille Language Server**:
- [ ] Pas d'erreurs
- [ ] Messages de démarrage corrects
- [ ] Connexion client/serveur établie

---

## 🎉 Scénario de Démo Complet

### Scénario: Créer un Counter depuis zéro

1. **Nouveau fichier:**
   ```bash
   code test.rat
   ```

2. **Snippet counter:**
   - Taper `counter` + Tab
   - Template complet généré ✓

3. **Ajouter main:**
   - Taper `main` + Tab
   - Template main généré ✓

4. **Spawn counter:**
   ```ratatouille
   proc main() {
       let c = spa  # Ctrl+Space → spawn suggéré ✓
   ```

5. **Auto-complétion proc:**
   ```ratatouille
   let c = spawn Cou  # Ctrl+Space → Counter suggéré ✓
   ```

6. **Envoyer message:**
   ```ratatouille
   c <- :  # Ctrl+Space → :increment, :decrement suggérés ✓
   ```

7. **Hover:**
   - Survoler `Counter` → Documentation affichée ✓

8. **Go to Definition:**
   - F12 sur `Counter` → Saute à définition ✓

**Résultat:** Un counter fonctionnel créé en 2 minutes avec aide complète de l'IDE! 🎉

---

## 📝 Rapport de Test

### Template de Rapport

```markdown
## Test Report - Ratatouille Extension v2.0

Date: _________
Testeur: _________

### Environnement
- OS: _________
- VS Code version: _________
- Node.js version: _________

### Résultats

| Fonctionnalité | Status | Commentaires |
|----------------|--------|--------------|
| Syntax Highlighting | ☐ Pass ☐ Fail | |
| Auto-completion | ☐ Pass ☐ Fail | |
| Hover | ☐ Pass ☐ Fail | |
| Go to Definition | ☐ Pass ☐ Fail | |
| Snippets | ☐ Pass ☐ Fail | |
| Configuration | ☐ Pass ☐ Fail | |

### Bugs Trouvés
1. ...
2. ...

### Recommandations
- ...
```

---

**Happy Testing! 🧪✨**
