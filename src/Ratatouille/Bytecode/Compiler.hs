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
  ELiteral lit -> compileLiteral lit

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
    compileBinaryOp op left right

  -- If-then-else expression
  EIf condition thenBranch elseBranch -> compileIf condition thenBranch elseBranch
  
  -- Type casting
  ECast castType targetType castExpr -> compileCast castType targetType castExpr
  
  -- Function/procedure call
  ECall funcName args -> compileCall funcName args
  
  -- Spawn process
  ESpawn procNamePat args -> compileSpawn procNamePat args
  
  -- Send message
  ESend receiver message -> compileSend receiver message
  
  -- Receive block
  EReceive cases -> compileReceiveBlock cases
  
  -- Block with statements
  EBlock statements finalExpr -> compileBlock statements finalExpr
  
  -- Monad bind operations
  EMaybeBind monadExpr funcExpr -> compileMaybeBind monadExpr funcExpr
  EEitherBind monadExpr funcExpr -> compileEitherBind monadExpr funcExpr
  
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

  -- Assignment expression (returns the assigned value)
  EAssign varName assignExpr -> compileExpr assignExpr ++ [STORE_LOCAL varName, LOAD_LOCAL varName]
  
  -- Field access
  EFieldAccess targetExpr fieldName -> compileExpr targetExpr ++ [GET_FIELD fieldName]
  
  -- Self reference (current process PID)
  ESelf -> [SELF]

-- | Compile a statement to bytecode
compileStmt :: Stmt -> Bytecode
compileStmt stmt = case stmt of
  -- Let bindings use local variables in process context (type annotation is ignored for now)
  SLet varName maybeType expr -> compileLet varName maybeType expr
  
  -- Destructuring let statements
  SLetPattern pattern expr -> compileExpr expr ++ compileDestructure pattern
  
  -- Const bindings (treated the same as let in bytecode, const enforcement happens at runtime)
  SConst varName maybeType expr -> compileLet varName maybeType expr
  
  -- Assignment statements
  SAssign varName expr -> compileAssign varName expr
  
  -- Expression statements - handle state assignments specially
  SExpr expr -> compileExprStmt expr

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

-- | Compile parameter bindings for process body
compileParamBindings :: [Text] -> Bytecode
compileParamBindings params = concatMap (\param -> [STORE_LOCAL param]) (reverse params)

-- | Compile state initialization for process body
compileStateInit :: Maybe Expr -> Bytecode
compileStateInit maybeState = case maybeState of
  Nothing -> [PUSH_UNIT, INIT_STATE]  -- Default empty state
  Just stateExpr -> compileExpr stateExpr ++ [INIT_STATE]

-- | Advanced process body compilation with parameter binding
compileProcBodyAdvanced :: [Text] -> ProcBody -> Bytecode
compileProcBodyAdvanced params (ProcBody maybeState receiveCases) =
  -- 1. Bind parameters to local variables (stack has args in reverse order)
  let paramBindings = compileParamBindings params
      
      -- 2. Initialize state if provided
      stateInit = compileStateInit maybeState
      
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

  PLiteral lit -> compileLiteralPattern lit

  PAtom atomName -> [MATCH_ATOM atomName 2]  -- Jump 2 instructions if no match

  PTuple patterns -> compileTuplePattern patterns

  -- Array patterns: similar to tuples but for arrays
  PArray patterns -> compileArrayPattern patterns

  -- Variadic patterns: capture remaining elements
  -- For now, compile as a simple variable that captures all remaining
  PVarargs varName -> [MATCH_VAR varName]  -- Simplified implementation

-- =============================================================================
-- HELPER FUNCTIONS
-- =============================================================================

-- | Compile literal pattern matching
compileLiteralPattern :: Literal -> Bytecode
compileLiteralPattern lit = case lit of
  LInt n -> [PUSH_INT n, MATCH_ATOM (pack $ show n) 2]
  LTypedInt _ n -> [PUSH_INT n, MATCH_ATOM (pack $ show n) 2]
  LFloat f -> [PUSH_FLOAT f, MATCH_ATOM (pack $ show f) 2]
  LTypedFloat _ f -> [PUSH_FLOAT f, MATCH_ATOM (pack $ show f) 2]
  LString s -> [PUSH_STRING s, MATCH_ATOM s 2]
  LBool b -> [PUSH_INT (if b then 1 else 0), MATCH_ATOM (if b then pack "true" else pack "false") 2]
  LNone -> [PUSH_NONE, MATCH_ATOM (pack "none") 2]

-- | Compile tuple pattern matching
compileTuplePattern :: [Pattern] -> Bytecode
compileTuplePattern patterns =
  let numElements = length patterns
      subPatterns = concatMap compilePattern patterns
      jumpOffset = length subPatterns + 1
  in [MATCH_TUPLE numElements jumpOffset] ++ subPatterns

-- | Compile array pattern matching
compileArrayPattern :: [Pattern] -> Bytecode
compileArrayPattern patterns =
  let numElements = length patterns
      subPatterns = concatMap compilePattern patterns
      jumpOffset = length subPatterns + 1
  in [MATCH_TUPLE numElements jumpOffset] ++ subPatterns  -- Reuse MATCH_TUPLE for now

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

-- | Compile pattern extraction for destructuring
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

-- | Compile literal values
compileLiteral :: Literal -> Bytecode
compileLiteral lit = case lit of
  LInt n -> [PUSH_INT n]
  LTypedInt _ n -> [PUSH_INT n]
  LFloat f -> [PUSH_FLOAT f]
  LTypedFloat _ f -> [PUSH_FLOAT f]
  LString s -> [PUSH_STRING s]
  LBool b -> [PUSH_INT (if b then 1 else 0)]
  LNone -> [PUSH_NONE]

-- | Compile binary operations
compileBinaryOp :: Op -> Expr -> Expr -> Bytecode
compileBinaryOp op left right =
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

-- | Compile if-then-else expressions
compileIf :: Expr -> Expr -> Maybe Expr -> Bytecode
compileIf condition thenBranch elseBranch =
  let condCode = compileExpr condition
      thenCode = compileExpr thenBranch
      elseCode = maybe [] compileExpr elseBranch
      thenJump = if null elseCode then [] else [JUMP (length elseCode)]
  in condCode ++ [JUMP_IF_FALSE (length thenCode + length thenJump)] ++ thenCode ++ thenJump ++ elseCode

-- | Compile type casting
compileCast :: CastType -> Type -> Expr -> Bytecode
compileCast castType targetType castExpr =
  let exprCode = compileExpr castExpr
      typeStr = typeToText targetType
      castInstr = case castType of
        StaticCast -> STATIC_CAST typeStr
        ReinterpretCast -> REINTERPRET_CAST typeStr
        ConstCast -> CONST_CAST
  in exprCode ++ [castInstr]

-- | Compile function calls
compileCall :: Text -> [Expr] -> Bytecode
compileCall funcName args = concatMap compileExpr args ++ [CALL funcName]

-- | Compile process spawning
compileSpawn :: Text -> [Expr] -> Bytecode
compileSpawn procNameArg args = concatMap compileExpr args ++ [CREATE_INSTANCE procNameArg]

-- | Compile message sending
compileSend :: Expr -> Expr -> Bytecode
compileSend receiver message = compileExpr receiver ++ compileExpr message ++ [SEND]

-- | Compile block expressions
compileBlock :: [Stmt] -> Expr -> Bytecode
compileBlock statements finalExpr = concatMap compileStmt statements ++ compileExpr finalExpr

-- | Compile Maybe bind operations
compileMaybeBind :: Expr -> Expr -> Bytecode
compileMaybeBind monadExpr (EVar funcName) = compileExpr monadExpr ++ [MAYBE_BIND funcName]
compileMaybeBind monadExpr funcExpr = compileExpr monadExpr ++ compileExpr funcExpr ++ [MAYBE_BIND (pack "anonymous")]

-- | Compile Either bind operations
compileEitherBind :: Expr -> Expr -> Bytecode
compileEitherBind monadExpr (EVar funcName) = compileExpr monadExpr ++ [EITHER_BIND funcName]
compileEitherBind monadExpr funcExpr = compileExpr monadExpr ++ compileExpr funcExpr ++ [EITHER_BIND (pack "anonymous")]

-- | Compile let statement
compileLet :: Text -> Maybe Type -> Expr -> Bytecode
compileLet varName _maybeType expr = compileExpr expr ++ [STORE_LOCAL varName]

-- | Compile assignment statement
compileAssign :: Text -> Expr -> Bytecode
compileAssign varName expr = compileExpr expr ++ [STORE_LOCAL varName]

-- | Compile state assignment for special state variable
compileStateAssign :: Op -> Expr -> Bytecode
compileStateAssign op rightExpr =
  let opInstr = case op of
        Add -> ADD
        Sub -> SUB
        Mul -> MUL
        Div -> DIV
        _ -> error "Unsupported state assignment operator"
  in [GET_STATE] ++ compileExpr rightExpr ++ [opInstr, SET_STATE]

-- | Compile expression statement
compileExprStmt :: Expr -> Bytecode
compileExprStmt expr = case expr of
  -- State assignment: state = newValue (using binary ops as assignments)
  EBinOp op (EVar state) rightExpr | state == pack "state" ->
    compileStateAssign op rightExpr
  -- General expression
  _ -> compileExpr expr

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
  I8  -> pack "i8"
  I16 -> pack "i16"
  I32 -> pack "i32"
  I64 -> pack "i64"
  U8  -> pack "u8"
  U16 -> pack "u16"
  U32 -> pack "u32"
  U64 -> pack "u64"
  F32 -> pack "f32"
  F64 -> pack "f64"
