### NEXUS_CONCEPT.md

# Nexus : Un Langage Orienté Processus

## 1\. La Philosophie 🚀

Nexus n'est pas un langage fonctionnel classique. Sa philosophie est simple et puissante : **"Tout est un processus isolé qui communique par messages."**

Imaginez votre programme non pas comme une série de calculs, mais comme une ruche 🐝. Chaque abeille est un **processus** : une entité autonome avec sa propre mémoire et sa propre tâche. Les abeilles ne peuvent pas lire dans les pensées des autres (accéder à leur mémoire). Elles ne peuvent que communiquer en se passant des messages.

Ce modèle, inspiré par Erlang/Elixir, offre nativement une **sécurité** et une **robustesse** exceptionnelles.

-----

## 2\. Les Concepts Clés

### a. Le Processus (`proc`)

Un `proc` est un **plan** pour créer des processus. Il définit deux choses :

  * **L'état interne (`state`)** : La mémoire privée du processus. Personne d'autre ne peut y toucher.
  * **Le comportement (`receive`)** : Une liste de "patterns" de messages auxquels le processus sait réagir.

<!-- end list -->

```nexus
// Le plan pour un processus 'Compteur'
proc Counter(initial_value) {
  // 1. Son état initial et privé
  state: initial_value,

  // 2. Les messages qu'il comprend
  receive {
    | :increment -> state = state + 1
    | :get(sender) -> sender <- state
  }
}
```

### b. La Création (`spawn`)

On utilise `spawn` pour créer une instance d'un processus à partir de son plan. `spawn` ne retourne pas le processus lui-même, mais son **PID (Process Identifier)**. Le PID est comme une adresse email ou un numéro de téléphone : c'est la seule façon de contacter ce processus.

```nexus
// Crée un compteur qui commence à 0.
// 'pid_counter' contient l'adresse unique de notre nouveau processus.
let pid_counter = spawn Counter(0)
```

### c. L'Envoi de Messages (`<-`)

C'est le cœur de l'interaction. L'opérateur `<-` envoie un message à la "boîte aux lettres" d'un PID. L'envoi est **asynchrone** : votre code n'attend pas de réponse, il continue son exécution immédiatement.

```nexus
pid_counter <- :increment // Envoie le message :increment
pid_counter <- :increment // Le compteur est maintenant à 2

// Pour demander la valeur, on doit lui donner notre propre adresse ('self')
// pour qu'il sache à qui répondre.
pid_counter <- :get(self)
```

### d. La Réception de Messages (`receive`)

Un bloc `receive` met le processus courant en **pause**. Il attend qu'un message arrive dans sa boîte aux lettres, puis utilise le **pattern matching** pour trouver le code à exécuter. Si aucun message ne correspond, il attend le suivant.

```nexus
// Le processus principal se met en attente d'une réponse.
receive {
  // Si un message arrive, il est capturé dans 'valeur'
  | valeur -> print("Le compteur a répondu : " ++ valeur)
}
```

-----

## 3\. Exemple Complet : Le Compte en Banque Sécurisé

Cet exemple montre comment l'isolation garantit la sécurité des données. Le solde du compte est inaccessible directement.

```nexus
// Le plan du compte en banque
proc BankAccount(balance) {
  state: balance,

  receive {
    | { :deposit, amount } -> state = state + amount
    | { :withdraw, amount } ->
        if state >= amount then
          state = state - amount
    | { :get_balance, sender } ->
        sender <- { state }
  }
}

// --- Utilisation ---
let my_account = spawn BankAccount(100)

my_account <- { :deposit, 50 } // Solde = 150
my_account <- { :withdraw, 200 } // Action ignorée, solde reste à 150

// On demande le solde
my_account <- { :get_balance, self }

// On attend la réponse
receive {
  | { final_balance } -> print("Solde final : " ++ final_balance)
}
```

Ce modèle nous force à construire des systèmes robustes et sécurisés par conception.
