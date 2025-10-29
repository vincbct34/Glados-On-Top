## Syntaxe du Ratatouille

# Conventions de fichiers

**Extension** : `<filename>.rat`

**Filename** : `PascalCase`

# Commentaires

Les commentaires commencent par `#` :

```
# Ceci est un commentaire
```

# Processus

**Name case** : `PascalCase`

Un processus (proc) est l’unité de base de Ratatouille. C’est un acteur qui peut recevoir et envoyer des messages.

## Définition d’un processus

```
proc MyProcess()
{
    receive {
        # -> pattern matching ici <-
    }
}
```

## Processus avec paramètres

```
proc Counter(initial)
{
    state: initial,
    receive {
        # -> pattern matching ici <-
    }
}
```

## State (État)

Chaque processus peut avoir une variable `state` :
- Elle est initialisée avec une valeur par défaut ou via les arguments du processus
- Elle est accessible dans tout le scope du processus
- Elle peut être modifiée avec `state = nouvelle_valeur`

```
proc Counter(initial) {
    state: initial,
    receive {
        | :increment -> state = state + 1
        | :reset -> state = 0
    }
}
```

## Receive (Réception de messages)

Le bloc `receive` définit les patterns de messages que le processus peut recevoir.

### Pattern matching simple

```
proc Greeter()
{
    receive {
        | :hello   -> print("Hello")
        | :goodbye -> print("bye")
    }
}
```

### Pattern avec multiples paramètres

```
proc Calculator() {
    state: 0,
    receive {
        | { :add, x, sender } -> {
            let result = state + x
            state = result
            sender <- :result(result)
        }
        | { :divide, a, b, sender } -> {
            if b == 0 then {
                sender <- :error("division_by_zero")
            } else {
                sender <- :success(a / b)
            }
        }
    }
}
```

### Pattern avec spread operator (…)

```
receive {
    | { :error, type, details... } -> print("Erreur " ++ type ++ ": " ++ details)
}
```

## Instantiation d’un processus

Utiliser `spawn` pour créer une instance d’un processus :

```
let myVar = spawn MyProcess()
let counter = spawn Counter(0)
```

# Messages

## Envoyer un message

Utiliser l’opérateur `<-` pour envoyer un message à un processus :

```
process <- :message_simple
process <- :message_avec_parametres(value1, value2)
```

## Recevoir des messages (dans le code principal)

```
receive {
    | :success(result) -> print("Succès: " + result)
    | :error(type)     -> print("Erreur: " + type)
}
```

## Self

`self` représente le processus actuel. Utilisé pour :
- S’envoyer un message à soi-même
- Passer sa propre référence à un autre processus

```
self <- :increment(sender)
sender <- :reply(self)
```

# Variables

## Déclaration

```
let myVar = valeur
let result = state + x
```

## Types de valeurs

- **Nombres** : `0`, `42`, `3.14`
- **Strings** : `"Hello"`, `"Error message"`
- **Booléens** : `true`, `false`
- **Atoms** : `:increment`, `:hello`, `:error`
- **Tuples** : `{ :atom, value1, value2 }`
- **Processus** : Résultat de `spawn`
- **None** : `none`

# Structures de contrôle

## If-then-else

```
let condition = true

if condition then {
    # code si vrai
} else {
    # code si faux
}
```

Exemple :

```
if b == 0 then {
    sender <- :error("division_by_zero")
} else {
    sender <- :success(a / b)
}
```

## If imbriqués

```
if name == "A" then {
    state = { pid, none, none }
} else {
    if name == "B" then {
        state = { none, pid, none }
    }
}
```

# Opérateurs

## Opérateurs arithmétiques

- `+` : addition
- `-` : soustraction
- `+=` : addition et assignation
- `-=` : soustraction et assignation
- `++` : incrémentation de 1
- `--` : décrémentation de 1
- `*` : multiplication
- `/` : division
- `%` : reste

## Opérateurs de comparaison

- `==` : égalité
- `<` : inférieur à
- `<=` : inférieur ou égal à
- `>` : supérieur à
- `>=` : supérieur ou égal à

## Opérateur d’inversion

- `not`
    
    Exemple :
    
    ```
    let foo = true
    
    if not foo {
    	print("foo is false !")
    }
    ```
    

## Opérateur de concaténation

- `+` : concaténation de strings ou de listes

```
print("Résultat: " + value)

let myList = [1, 2, 3] + [4, 5, 6]
```

# Fonctions intégrées

## print

Affiche une valeur :

```
print("Hello World")
print("Valeur: " + state)
```

# Blocs de code

Les blocs sont délimités par des accolades `{ }` :

```
| :message -> {
    state = state + 1
    sender <- :reply(state)
}
```

## Séquence d’instructions

Les instructions sont séparées par des retours à la ligne :

```
state += 1
sender <- :reply(state)
print("Done")
```

# Exemples complets

## Processus simple

```
proc Greeter()
{
    receive {
        | :hello(sender)   -> print("Hello")
        | :goodbye(sender) -> print("bye")
    }
}

let g = spawn Greeter()
g <- :hello()
```

## Processus avec état

```
proc Counter(initial) {
    state: initial,
    receive {
        | :increment   -> state++
        | :decrement   -> state--
        | :get(sender) -> sender <- state
        | :reset       -> state = 0
    }
}
```

## Communication entre processus

```
proc Router() {
    state: [],
    receive {
        | :register(name, pid)      -> # Enregistrer le processus
        | :route(from, to, message) -> print("Routing message from " ++ from ++ " to " ++ to)
    }
}

proc Node(name, router) {
    state: 0,
    receive {
        | :start -> router <- :register(name, self)
    }
}

let router = spawn Router()
let nodeA = spawn Node("A", router)
nodeA <- :start
```