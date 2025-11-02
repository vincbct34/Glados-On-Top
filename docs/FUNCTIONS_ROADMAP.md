# 🚀 ROADMAP: Fonctions Pures avec fn main()

**Date de début:** 31 Octobre 2025  
**Date de complétion Phase 1-6:** 31 Octobre 2025  
**Objectif:** Ajouter `fn` pour fonctions pures avec restrictions strictes  
**Durée estimée:** ~7 heures (1 journée)  
**Principe:** Fonctions = helpers purs UNIQUEMENT, Processes = cœur du langage

---

## ✅ STATUS: Phases 1-6 COMPLETES

**Implémentation terminée:**
- ✅ Phase 1: AST étendu avec FuncDefinition
- ✅ Phase 2: Parser avec validation `fn main()` obligatoire
- ✅ Phase 3: Instructions bytecode (DEFINE_FUNCTION 0x64, CALL_FUNCTION 0x65, RETURN)
- ✅ Phase 4-5: Compilation complète avec compileFunctionBody
- ✅ Phase 6: Runtime VM avec support DEFINE_FUNCTION, CALL_FUNCTION, RETURN

**Tests validés:**
- ✅ `simple_function.rat` - Double (21) = 42
- ✅ `minimal_main.rat` - Main obligatoire fonctionne
- ✅ `factorial.rat` - Récursion (5! = 120)
- ✅ `no_main_error.rat` - Erreur si pas de main

**Reste à faire:**
- ❌ Phase 4: Validation des restrictions (pas spawn/send/receive dans fn)
- ❌ Phase 7: Tests complets et edge cases

---

## 🎯 Vision Finale

```ratatouille
// ============= FONCTIONS (calculs purs) =============
fn factorial(n) {
    if n <= 1 then 1 else n * factorial(n - 1)
}

fn formatMessage(user, text) {
    "[" ++ user ++ "]: " ++ text
}

// ============= PROCESSES (comportement actor) =============
proc Logger() {
    receive {
        | msg -> {
            print(msg)
            self <- :continue
        }
        | :continue -> {}
    }
}

// ============= MAIN (point d'entrée obligatoire) =============
fn main() {
    let result = factorial(5)
    print(result)
    
    let logger = spawn Logger()
    logger <- "Server started"
    
    0  // Exit code
}
```

---

## 📋 Restrictions Strictes des Fonctions

### ✅ Autorisé dans `fn`:
- Calculs arithmétiques: `a + b`, `n * 2`
- Conditionnels: `if/then/else`
- Pattern matching: `match expr { ... }`
- Appels de fonctions: `factorial(5)`
- Opérations sur données: transformations, validation, formatage
- Récursion

### ❌ INTERDIT dans `fn`:
- `spawn` - Pas de création de processes
- `send` / `<-` - Pas d'envoi de messages
- `receive` - Pas de réception de messages
- `state` - Pas d'état mutable
- Effets de bord (I/O limité)

**Règle:** Si ça touche aux actors/concurrence → **utiliser `proc`**

---

## 🗂️ Plan d'Implémentation (7 phases)

### Phase 0: Préparation (15 min)
- [x] Créer cette roadmap
- [ ] Backup du code actuel
- [ ] Créer branche `feature/functions`

---

### Phase 1: AST (45 min)

#### Fichier: `src/Ratatouille/AST.hs`

**Objectif:** Ajouter `FuncDefinition` à l'AST

#### Tâches:
- [ ] **1.1** Ajouter type `FuncDefinition`
  ```haskell
  data FuncDefinition = FuncDef
    { funcName :: Text,
      funcParams :: [Text],
      funcBody :: Expr  -- Juste une expression, pas de ProcBody
    }
    deriving (Show, Eq)
  ```

- [ ] **1.2** Modifier `Definition` pour inclure `DFunc`
  ```haskell
  data Definition
    = DProc ProcDefinition
    | DFunc FuncDefinition  -- NOUVEAU
    | DStmt Stmt
    | DImport ImportDecl
    deriving (Show, Eq)
  ```

- [ ] **1.3** Exporter les nouveaux types dans le module

**Validation:** Code compile sans erreur

**Temps estimé:** 45 min

---

### Phase 2: Parser (1h30)

#### Fichier: `src/Ratatouille/Parser/Proc.hs`

**Objectif:** Parser la syntaxe `fn name(params) { body }`

#### Tâches:

- [ ] **2.1** Créer parser pour fonctions (20 min)
  ```haskell
  pFuncDef :: Parser FuncDefinition
  pFuncDef = do
    _ <- symbol (pack "fn")
    name <- pIdentifier
    params <- between (symbol (pack "(")) (symbol (pack ")")) pFuncParams
    body <- between (symbol (pack "{")) (symbol (pack "}")) pExpr
    return $ FuncDef name params body
  
  pFuncParams :: Parser [Text]
  pFuncParams = sepEndBy pIdentifier (symbol (pack ","))
  ```

- [ ] **2.2** Modifier `pDefinition` pour inclure fonctions (10 min)
  ```haskell
  pDefinition :: Parser Definition
  pDefinition = 
    (DImport <$> pImport) <|>
    (try $ DFunc <$> pFuncDef) <|>  -- NOUVEAU (avant proc!)
    (DProc <$> pProcDef) <|> 
    (DStmt <$> pTopLevelStatement)
  ```

- [ ] **2.3** Validation de `main` (30 min)
  ```haskell
  pProgram :: Parser Program
  pProgram = do
    sc
    definitions <- many (pDefinition <* optional (symbol (pack ";")))
    eof
    
    -- Vérifier présence de main
    let mainFunc = find isMainFunc definitions
    case mainFunc of
      Nothing -> fail "Program must contain a 'fn main()' function"
      Just _ -> return $ Program definitions
    where
      isMainFunc (DFunc (FuncDef name params _)) = 
        name == pack "main" && null params
      isMainFunc _ = False
  ```

- [ ] **2.4** Tester le parser (30 min)
  - Créer `test_parser_func.rat` avec exemples
  - Tester parsing réussi
  - Tester erreur si pas de main

**Validation:** Parser reconnaît syntaxe `fn` et valide `main`

**Temps estimé:** 1h30

---

### Phase 3: Bytecode Instructions (30 min)

#### Fichier: `src/Ratatouille/Bytecode/Types.hs`

**Objectif:** Ajouter instructions pour fonctions

#### Tâches:

- [ ] **3.1** Ajouter instructions (10 min)
  ```haskell
  data Instruction
    = -- ... instructions existantes ...
    
    -- Instructions pour fonctions
    | DEFINE_FUNCTION Text [Text] Bytecode  -- Définit une fonction
    | CALL_FUNCTION Text Int                -- Appelle une fonction (nom, nb args)
    | RETURN                                -- Retourne d'une fonction
    
    deriving (Show, Eq)
  ```

- [ ] **3.2** Ajouter opcodes (10 min)
  ```haskell
  -- Dans Encoder.hs
  DEFINE_FUNCTION {} -> 0x80
  CALL_FUNCTION {} -> 0x81
  RETURN -> 0x82
  ```

- [ ] **3.3** Ajouter décodage (10 min)
  ```haskell
  -- Dans Decoder.hs
  0x80 -> DEFINE_FUNCTION ...
  0x81 -> CALL_FUNCTION ...
  0x82 -> RETURN
  ```

**Validation:** Types compilent, opcodes définis

**Temps estimé:** 30 min

---

### Phase 4: Validation des Fonctions (1h)

#### Fichier: `src/Ratatouille/Bytecode/Compiler.hs`

**Objectif:** Valider que les fonctions ne contiennent pas spawn/send/receive

#### Tâches:

- [ ] **4.1** Créer fonction de validation (40 min)
  ```haskell
  -- Valider qu'une expression est "pure" (pas d'actor operations)
  validatePureExpr :: Expr -> Either String ()
  validatePureExpr expr = case expr of
    ESpawn _ _ -> Left "ERROR: Functions cannot spawn processes. Use 'proc' instead."
    ESend _ _ -> Left "ERROR: Functions cannot send messages (<-). Use 'proc' instead."
    EReceive _ -> Left "ERROR: Functions cannot receive messages. Use 'proc' instead."
    
    -- Récursion sur sous-expressions
    EBinOp _ e1 e2 -> do
      validatePureExpr e1
      validatePureExpr e2
    
    EUnaryOp _ e -> validatePureExpr e
    
    EIf cond thenBr elseBr -> do
      validatePureExpr cond
      validatePureExpr thenBr
      maybe (Right ()) validatePureExpr elseBr
    
    EBlock stmts resultExpr -> do
      mapM_ validatePureStmt stmts
      validatePureExpr resultExpr
    
    ETuple exprs -> mapM_ validatePureExpr exprs
    EArray exprs -> mapM_ validatePureExpr exprs
    EIndex e1 e2 -> validatePureExpr e1 >> validatePureExpr e2
    
    ECall _ args -> mapM_ validatePureExpr args
    EAssign _ e -> validatePureExpr e
    
    EMatch scrutinee cases -> do
      validatePureExpr scrutinee
      mapM_ (\(MatchCase _ e) -> validatePureExpr e) cases
    
    -- Cas safe
    EVar _ -> Right ()
    ELiteral _ -> Right ()
    EAtom _ -> Right ()
    ESelf -> Right ()
    EFieldAccess e _ -> validatePureExpr e
    _ -> Right ()
  
  validatePureStmt :: Stmt -> Either String ()
  validatePureStmt (SLet _ expr) = validatePureExpr expr
  validatePureStmt (SConst _ expr) = validatePureExpr expr
  validatePureStmt (SExpr expr) = validatePureExpr expr
  ```

- [ ] **4.2** Tester la validation (20 min)
  - Créer tests avec spawn/send/receive → erreur attendue
  - Créer tests valides → compilation OK

**Validation:** Erreurs claires si violation des règles

**Temps estimé:** 1h

---

### Phase 5: Compilation des Fonctions (1h30)

#### Fichier: `src/Ratatouille/Bytecode/Compiler.hs`

**Objectif:** Compiler les fonctions en bytecode

#### Tâches:

- [ ] **5.1** Compiler définition de fonction (30 min)
  ```haskell
  compileDefinition :: Definition -> Either String Bytecode
  compileDefinition def = case def of
    -- Process (existant)
    DProc (ProcDef pName pParams pBody) -> do
      let processBodyCode = compileProcBodyAdvanced pParams pBody
      return [DEFINE_PROCESS pName pParams processBodyCode]
    
    -- Fonction (NOUVEAU)
    DFunc (FuncDef fName fParams fBody) -> do
      -- 1. Valider pureté
      validatePureExpr fBody
      
      -- 2. Compiler
      let funcBodyCode = compileFunctionBody fParams fBody
      return [DEFINE_FUNCTION fName fParams funcBodyCode]
    
    -- Reste inchangé
    DStmt stmt -> return $ compileStmt stmt
    DImport _ -> return []
  ```

- [ ] **5.2** Compiler corps de fonction (40 min)
  ```haskell
  compileFunctionBody :: [Text] -> Expr -> Bytecode
  compileFunctionBody params body =
    -- 1. Bind parameters (stack a les args en ordre inverse)
    let paramBindings = concatMap (\param -> [STORE_LOCAL param]) (reverse params)
        
        -- 2. Compile body
        bodyCode = compileExpr body
        
        -- 3. Return
        returnCode = [RETURN]
        
    in paramBindings ++ bodyCode ++ returnCode
  ```

- [ ] **5.3** Compiler appel de fonction (20 min)
  ```haskell
  compileExpr :: Expr -> Bytecode
  compileExpr expr = case expr of
    -- ... cas existants ...
    
    -- Modifier ECall pour supporter fonctions ET processes
    ECall funcName args ->
      let compiledArgs = concatMap compileExpr args
          argCount = length args
      in compiledArgs ++ [CALL_FUNCTION funcName argCount]
  ```

**Validation:** Fonctions compilent en bytecode correct

**Temps estimé:** 1h30

---

### Phase 6: Runtime VM (1h30)

#### Fichier: `src/Ratatouille/VM/Interpreter.hs`

**Objectif:** Exécuter les instructions de fonctions

#### Tâches:

- [ ] **6.1** Gérer DEFINE_FUNCTION (20 min)
  ```haskell
  executeInstruction :: Instruction -> VM ()
  executeInstruction instr = case instr of
    -- ... instructions existantes ...
    
    DEFINE_FUNCTION name params body -> do
      -- Enregistrer la fonction dans l'environnement global
      modify $ \vm -> vm { vmFunctions = Map.insert name (params, body) (vmFunctions vm) }
  ```

- [ ] **6.2** Gérer CALL_FUNCTION (50 min)
  ```haskell
  CALL_FUNCTION name argCount -> do
    -- 1. Pop arguments de la stack
    args <- replicateM argCount popStack
    
    -- 2. Chercher la fonction
    funcs <- gets vmFunctions
    case Map.lookup name funcs of
      Nothing -> throwError $ "Function not found: " <> name
      Just (params, body) -> do
        -- 3. Sauvegarder contexte (call stack frame)
        oldLocals <- gets vmLocals
        oldPC <- gets vmPC
        
        -- 4. Créer nouveau frame avec paramètres
        let newLocals = Map.fromList (zip params (reverse args))
        modify $ \vm -> vm { vmLocals = newLocals }
        
        -- 5. Exécuter le corps de la fonction
        executeBytecode body
        
        -- 6. Restaurer contexte
        modify $ \vm -> vm { vmLocals = oldLocals, vmPC = oldPC }
  ```

- [ ] **6.3** Gérer RETURN (10 min)
  ```haskell
  RETURN -> do
    -- La valeur de retour est déjà sur la stack
    -- Juste interrompre l'exécution du bytecode actuel
    modify $ \vm -> vm { vmReturning = True }
  ```

- [ ] **6.4** Modifier structure VM (10 min)
  ```haskell
  data VMState = VMState
    { -- ... champs existants ...
    , vmFunctions :: Map Text ([Text], Bytecode)  -- NOUVEAU
    , vmReturning :: Bool                          -- NOUVEAU (pour RETURN)
    }
  ```

**Validation:** Fonctions s'exécutent correctement

**Temps estimé:** 1h30

---

### Phase 7: Tests et Exemples (1h)

#### Objectif: Valider le système avec des tests complets

#### Tâches:

- [ ] **7.1** Créer tests unitaires (30 min)
  ```ratatouille
  // test_functions.rat
  fn double(x) {
      x * 2
  }
  
  fn factorial(n) {
      if n <= 1 then 1 else n * factorial(n - 1)
  }
  
  fn main() {
      let a = double(21)
      print(a)  // 42
      
      let b = factorial(5)
      print(b)  // 120
      
      0
  }
  ```

- [ ] **7.2** Tester restrictions (15 min)
  ```ratatouille
  // test_func_restrictions.rat
  fn badFunction() {
      let p = spawn Counter(0)  // ❌ Doit échouer
      0
  }
  
  fn main() {
      badFunction()
  }
  ```

- [ ] **7.3** Exemple combiné fn + proc (15 min)
  ```ratatouille
  // test_fn_proc.rat
  fn formatLog(level, msg) {
      "[" ++ level ++ "] " ++ msg
  }
  
  proc Logger() {
      receive {
          | (:log, level, msg) -> {
              let formatted = formatLog(level, msg)
              print(formatted)
              self <- :continue
          }
          | :continue -> {}
      }
  }
  
  fn main() {
      let logger = spawn Logger()
      logger <- (:log, "INFO", "Server started")
      0
  }
  ```

**Validation:** Tous les tests passent

**Temps estimé:** 1h

---

## ✅ Checklist de Validation

### Après chaque phase:
- [ ] Code compile sans erreur
- [ ] Tests unitaires passent
- [ ] Documentation à jour
- [ ] Commit avec message clair

### Validation finale:
- [ ] `fn main()` obligatoire et validé
- [ ] Fonctions avec calculs purs fonctionnent
- [ ] Récursion fonctionne
- [ ] Restrictions (spawn/send/receive) appliquées
- [ ] Erreurs claires si violation
- [ ] Appels de fonctions fonctionnent
- [ ] Mix fn + proc fonctionne
- [ ] Exemples compilent et s'exécutent

---

## 🚨 Points d'Attention

### 1. **Order matters dans pDefinition**
```haskell
-- IMPORTANT: try $ DFunc avant DProc
-- Sinon "fn" peut être interprété comme identifier de proc
(try $ DFunc <$> pFuncDef) <|>
(DProc <$> pProcDef)
```

### 2. **Validation AVANT compilation**
```haskell
-- Valider pureté AVANT de compiler
DFunc (FuncDef fName fParams fBody) -> do
  validatePureExpr fBody  -- Peut échouer avec Either
  let funcBodyCode = compileFunctionBody fParams fBody
  return [DEFINE_FUNCTION fName fParams funcBodyCode]
```

### 3. **Call stack pour récursion**
```haskell
-- Sauvegarder/restaurer vmLocals pour chaque appel
-- Sinon variables locales écrasées lors de récursion
oldLocals <- gets vmLocals
-- ... exécution ...
modify $ \vm -> vm { vmLocals = oldLocals }
```

### 4. **RETURN interrompt l'exécution**
```haskell
-- Dans executeBytecode, vérifier vmReturning
executeBytecode :: Bytecode -> VM ()
executeBytecode bytecode = do
  forM_ bytecode $ \instr -> do
    returning <- gets vmReturning
    unless returning $ executeInstruction instr
```

---

## 📊 Estimation Temps Total

| Phase | Tâche | Durée |
|-------|-------|-------|
| 0 | Préparation | 15 min |
| 1 | AST | 45 min |
| 2 | Parser | 1h30 |
| 3 | Bytecode | 30 min |
| 4 | Validation | 1h |
| 5 | Compilation | 1h30 |
| 6 | Runtime VM | 1h30 |
| 7 | Tests | 1h |
| **TOTAL** | | **~7h30** |

---

## 🎯 Prochaine Étape

**COMMENCER PAR PHASE 1: Modifier l'AST**

```bash
# 1. Créer branche
git checkout -b feature/functions

# 2. Ouvrir AST.hs
code src/Ratatouille/AST.hs

# 3. Suivre étapes Phase 1
```

**Prêt à démarrer Phase 1 ?** 🚀
