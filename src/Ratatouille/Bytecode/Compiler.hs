{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- AST to Bytecode compiler for Ratatouille
-}

module Ratatouille.Bytecode.Compiler
  ( compileExpr,
    compileStmt,
    compileProgram,
    compileDefinition,
    compilePattern,
    compileReceiveBlock,
  )
where

import Data.Text (Text, pack)
import Ratatouille.AST
import Ratatouille.Bytecode.Types

-- | Compile an expression to bytecode
compileExpr :: Expr -> Bytecode
compileExpr expr = case expr of
  -- Literals
  ELiteral (LInt n) -> [PUSH_INT n]
  ELiteral (LTypedInt _ n) -> [PUSH_INT n]  -- Type info used for validation, value is the same
  ELiteral (LFloat f) -> [PUSH_FLOAT f]  -- Proper float support
  ELiteral (LTypedFloat _ f) -> [PUSH_FLOAT f]  -- Typed float
  ELiteral (LString s) -> [PUSH_STRING s]
  ELiteral (LBool b) -> [PUSH_INT (if b then 1 else 0)]  -- Boolean as 0 or 1
  ELiteral LNone -> [PUSH_NONE]

  -- Variables (distinguish between state, local vars, and global vars)
  EVar varName
    | varName == pack "state" -> [GET_STATE]
    | otherwise -> [LOAD_LOCAL varName]  -- Prefer local scope in processes

  -- Atoms
  EAtom atomName -> [PUSH_ATOM atomName]

  -- Tuples
  ETuple elements ->
    let compiledElements = concatMap compileExpr elements
        tupleInstruction = [PUSH_TUPLE (length elements)]
    in compiledElements ++ tupleInstruction

  -- Arrays
  EArray elements ->
    let compiledElements = concatMap compileExpr elements
        arrayInstruction = [PUSH_ARRAY (length elements)]
    in compiledElements ++ arrayInstruction

  -- Array indexing
  EIndex arrayExpr indexExpr ->
    let arrayCode = compileExpr arrayExpr
        indexCode = compileExpr indexExpr
    in arrayCode ++ indexCode ++ [INDEX]

  -- Binary operations
  EBinOp op left right ->
    let leftCode = compileExpr left
        rightCode = compileExpr right
        opCode = case op of
          Add -> [ADD]
          Sub -> [SUB]
          Mul -> [MUL]
          Div -> [DIV]
          Concat -> [CONCAT]
          Eq -> [CMP_EQ]
          Neq -> [CMP_NEQ]
          Lt -> [CMP_LT]
          Gt -> [CMP_GT]
          Lte -> [CMP_LTE]
          Gte -> [CMP_GTE]
          And -> [LOGIC_AND]
          Or -> [LOGIC_OR]
    in leftCode ++ rightCode ++ opCode

  -- If-then-else expression
  EIf condition thenBranch elseBranch ->
    let condCode = compileExpr condition
        thenCode = compileExpr thenBranch
        elseCode = maybe [] compileExpr elseBranch
        -- JUMP_IF_FALSE skips then branch if condition is false
        -- JUMP at end of then branch skips else branch
        thenJump = if null elseCode then [] else [JUMP (length elseCode)]
    in condCode ++ [JUMP_IF_FALSE (length thenCode + length thenJump)] ++ thenCode ++ thenJump ++ elseCode

  -- Field access
  EFieldAccess baseExpr fieldName ->
    compileExpr baseExpr ++ [GET_FIELD fieldName]

  -- Self keyword
  ESelf -> [SELF]
  
  -- Type casting
  ECast castType targetType castExpr ->
    let exprCode = compileExpr castExpr
        typeStr = typeToText targetType
        castInstr = case castType of
          StaticCast -> STATIC_CAST typeStr
          ReinterpretCast -> REINTERPRET_CAST typeStr
          ConstCast -> CONST_CAST  -- Removes const qualification, no type conversion needed
    in exprCode ++ [castInstr]
  
  -- Function/procedure call
  ECall funcName args ->
    let argsCode = concatMap compileExpr args
    in argsCode ++ [CALL funcName]
  
  -- Assignment
  EAssign varName value ->
    compileExpr value ++ [STORE_LOCAL varName]
  
  -- Spawn process - improved with proper instance creation
  ESpawn procName' args ->
    let argsCode = concatMap compileExpr args
        -- Arguments are now on stack, create instance will use them
    in argsCode ++ [CREATE_INSTANCE procName']
  
  -- Send message
  ESend receiver message ->
    let receiverCode = compileExpr receiver
        messageCode = compileExpr message
    in receiverCode ++ messageCode ++ [SEND]
  
  -- Receive block - now compiles to pattern matching bytecode
  EReceive cases -> compileReceiveBlock cases
  
  -- Block with statements
  EBlock statements finalExpr ->
    let statementsCode = concatMap compileStmt statements
        finalCode = compileExpr finalExpr
    in statementsCode ++ finalCode

  -- Maybe constructors
  EJust innerExpr -> compileExpr innerExpr ++ [PUSH_JUST]
  ENone -> [PUSH_NONE]
  
  -- Either constructors
  ELeft innerExpr -> compileExpr innerExpr ++ [PUSH_LEFT]
  ERight innerExpr -> compileExpr innerExpr ++ [PUSH_RIGHT]

  -- Increment/Decrement operators
  EPreInc varName -> [INC_VAR varName]
  EPostInc varName -> [INC_VAR_POST varName]
  EPreDec varName -> [DEC_VAR varName]
  EPostDec varName -> [DEC_VAR_POST varName]

-- | Compile a statement to bytecode
compileStmt :: Stmt -> Bytecode
compileStmt stmt = case stmt of
  -- Let bindings use local variables in process context (type annotation is ignored for now)
  SLet varName _maybeType expr -> compileExpr expr ++ [STORE_LOCAL varName]
  
  -- Destructuring let statements
  SLetPattern pattern expr -> compileExpr expr ++ compileDestructure pattern
  
  -- Const bindings (treated the same as let in bytecode, const enforcement happens at runtime)
  SConst varName _maybeType expr -> compileExpr expr ++ [STORE_LOCAL varName]
  
  -- Assignment statements
  SAssign varName expr -> compileExpr expr ++ [STORE_LOCAL varName]
  
  -- Expression statements - handle state assignments specially
  SExpr expr -> case expr of
    -- State assignment: state = newValue
    EBinOp Add (EVar state) rightExpr | state == pack "state" ->
      [GET_STATE] ++ compileExpr rightExpr ++ [ADD, SET_STATE]
    EBinOp Sub (EVar state) rightExpr | state == pack "state" ->
      [GET_STATE] ++ compileExpr rightExpr ++ [SUB, SET_STATE]
    EBinOp Mul (EVar state) rightExpr | state == pack "state" ->
      [GET_STATE] ++ compileExpr rightExpr ++ [MUL, SET_STATE]
    EBinOp Div (EVar state) rightExpr | state == pack "state" ->
      [GET_STATE] ++ compileExpr rightExpr ++ [DIV, SET_STATE]
    -- General assignment would need parser support for assignment operator
    _ -> compileExpr expr

-- | Compile a full program to bytecode
compileProgram :: Program -> Bytecode
compileProgram (Program definitions) = 
  concatMap compileDefinition definitions ++ [HALT]

-- | Compile a definition to bytecode
compileDefinition :: Definition -> Bytecode
compileDefinition def = case def of
  DProc (ProcDef pName pParams pBody) ->
    -- Generate process definition with proper parameter handling
    let processBodyCode = compileProcBodyAdvanced pParams pBody
    in [DEFINE_PROCESS pName pParams processBodyCode]
  
  DStmt stmt ->
    -- Top-level statement
    compileStmt stmt

-- | Advanced process body compilation with parameter binding
compileProcBodyAdvanced :: [Text] -> ProcBody -> Bytecode
compileProcBodyAdvanced params (ProcBody maybeState receiveCases) =
  -- 1. Bind parameters to local variables (stack has args in reverse order)
  let paramBindings = concatMap (\param -> [STORE_LOCAL param]) (reverse params)
      
      -- 2. Initialize state if provided
      stateInit = case maybeState of
        Nothing -> [PUSH_UNIT, INIT_STATE]  -- Default empty state
        Just stateExpr -> compileExpr stateExpr ++ [INIT_STATE]
      
      -- 3. Main process loop
      mainLoop = [PROCESS_LOOP] ++ compileReceiveBlock receiveCases
      
  in paramBindings ++ stateInit ++ mainLoop

-- | Compile receive block to explicit pattern matching bytecode
compileReceiveBlock :: [ReceiveCase] -> Bytecode
compileReceiveBlock cases =
  [WAIT_MESSAGE] ++ 
  concatMap compileReceiveCase cases ++
  [EXIT_PROCESS]  -- If no pattern matches, exit process

-- | Compile a single receive case
compileReceiveCase :: ReceiveCase -> Bytecode  
compileReceiveCase (Case pattern action) =
  let patternCode = compilePattern pattern
      actionCode = compileExpr action
      -- Jump back to WAIT_MESSAGE after executing action
  in patternCode ++ actionCode ++ [JUMP (-(length patternCode + length actionCode + 1))]

-- | Compile pattern matching to bytecode
compilePattern :: Pattern -> Bytecode
compilePattern pattern = case pattern of
  -- TODO: Optimize wildcards in tuples/arrays to skip extraction
  -- Example: {_, x, _} should only extract index 1, not 0 and 2
  -- This would reduce unnecessary stack operations in the VM
  PWildcard -> [MATCH_WILDCARD]

  PVar varName -> [MATCH_VAR varName]
  
  PVarTyped varName _maybeType _isConst -> [MATCH_VAR varName]  -- Type checking done separately

  PLiteral (LInt n) -> [PUSH_INT n, MATCH_ATOM (pack $ show n) 2]  -- Jump 2 if no match
  PLiteral (LTypedInt _ n) -> [PUSH_INT n, MATCH_ATOM (pack $ show n) 2]
  PLiteral (LFloat f) -> [PUSH_FLOAT f, MATCH_ATOM (pack $ show f) 2]
  PLiteral (LTypedFloat _ f) -> [PUSH_FLOAT f, MATCH_ATOM (pack $ show f) 2]
  PLiteral (LString s) -> [PUSH_STRING s, MATCH_ATOM s 2]
  PLiteral (LBool b) -> [PUSH_INT (if b then 1 else 0), MATCH_ATOM (if b then pack "true" else pack "false") 2]
  PLiteral LNone -> [PUSH_NONE, MATCH_ATOM (pack "none") 2]

  PAtom atomName -> [MATCH_ATOM atomName 2]  -- Jump 2 instructions if no match

  PTuple patterns ->
    let numElements = length patterns
        subPatterns = concatMap compilePattern patterns
        jumpOffset = length subPatterns + 1
    in [MATCH_TUPLE numElements jumpOffset] ++ subPatterns

  -- Array patterns: similar to tuples but for arrays
  PArray patterns ->
    let numElements = length patterns
        subPatterns = concatMap compilePattern patterns
        jumpOffset = length subPatterns + 1
    in [MATCH_TUPLE numElements jumpOffset] ++ subPatterns  -- Reuse MATCH_TUPLE for now

  -- Variadic patterns: capture remaining elements
  -- For now, compile as a simple variable that captures all remaining
  PVarargs varName -> [MATCH_VAR varName]  -- Simplified implementation

-- =============================================================================
-- HELPER FUNCTIONS
-- =============================================================================

-- | Convert a Type to Text representation for bytecode
typeToText :: Type -> Text
typeToText t = case t of
  TNumeric numType -> numericTypeToText numType
  TString -> pack "string"
  TBool -> pack "bool"
  TTuple types -> pack "{" <> mconcat (map (\ty -> typeToText ty <> pack ",") types) <> pack "}"
  TArray elemType Nothing -> pack "[" <> typeToText elemType <> pack "]"  -- Dynamic array/vector
  TArray elemType (Just size) -> pack "[" <> typeToText elemType <> pack "," <> pack (show size) <> pack "]"  -- Fixed-size array
  TMaybe innerType -> typeToText innerType <> pack "?"  -- Maybe type: T?
  TEither leftType rightType -> typeToText leftType <> pack "!" <> typeToText rightType  -- Either type: T!U
  TAtom -> pack "atom"
  TPid -> pack "pid"
  TNone -> pack "none"
  TVoid -> pack "void"
  TAny -> pack "any"

-- | Convert a NumericType to Text
numericTypeToText :: NumericType -> Text
numericTypeToText nt = case nt of
  I8 -> pack "i8"
  I16 -> pack "i16"
  I32 -> pack "i32"
  I64 -> pack "i64"
  U8 -> pack "u8"
  U16 -> pack "u16"
  U32 -> pack "u32"
  U64 -> pack "u64"
  F32 -> pack "f32"
  F64 -> pack "f64"

-- =============================================================================
-- PATTERN MATCHING COMPILATION
-- =============================================================================

-- | Compile destructuring pattern to bytecode
-- Assumes the value to destructure is on top of the stack
-- Generates code to extract and bind variables from the value
compileDestructure :: Pattern -> Bytecode
compileDestructure pattern = case pattern of
  PVar varName -> [STORE_LOCAL varName]
  
  PVarTyped varName _maybeType _isConst -> [STORE_LOCAL varName]  -- Type/const handled elsewhere
  
  PWildcard -> []  -- Ignore the value
  
  PTuple patterns ->
    -- For tuple destructuring: {a, b, c}
    -- Stack has tuple on top, we need to extract each element and bind
    concat [ compilePatternExtract i p | (i, p) <- zip [0..] patterns ]
  
  PArray patterns ->
    -- For array destructuring: [a, b, c]
    -- Similar to tuple
    concat [ compilePatternExtract i p | (i, p) <- zip [0..] patterns ]
  
  _ -> []  -- Other patterns not supported in destructuring yet
  where
    compilePatternExtract :: Integer -> Pattern -> Bytecode
    compilePatternExtract idx (PVar varName) = 
      [PUSH_INT idx, INDEX, STORE_LOCAL varName]
    compilePatternExtract idx (PVarTyped varName _typ _isConst) = 
      -- TODO: Add type checking for typed patterns
      -- TODO: Add const enforcement for const-qualified patterns
      [PUSH_INT idx, INDEX, STORE_LOCAL varName]
    -- TODO: Optimize wildcards - don't generate INDEX instruction for unused values
    compilePatternExtract _ PWildcard = []
    compilePatternExtract _ _ = []  -- Nested patterns TODO
