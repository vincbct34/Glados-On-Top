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

  -- Unary operations
  EUnaryOp op expr ->
    compileUnaryOp op expr

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
  
  -- Match expression (pattern matching on explicit value)
  EMatch matchExpr cases -> compileMatchExpr matchExpr cases
  
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
  EAssign varName assignExpr
    | varName == pack "state" -> compileExpr assignExpr ++ [SET_STATE, GET_STATE]  -- For state, set and return new value
    | otherwise -> compileExpr assignExpr ++ [STORE_LOCAL varName, LOAD_LOCAL varName]
  
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
  
  DImport _ ->
    -- Import handling is done at a higher level before compilation
    -- This should never be reached if imports are properly resolved
    []

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
      
      -- 3. Main process loop - just the receive block in a loop
      mainLoop = compileReceiveBlock receiveCases
      
  in paramBindings ++ stateInit ++ mainLoop

-- | Compile receive block to explicit pattern matching bytecode
compileReceiveBlock :: [ReceiveCase] -> Bytecode
compileReceiveBlock cases =
  [LABEL (pack "receive_loop"), WAIT_MESSAGE] ++ compileReceiveCasesSimple cases 0 ++ [EXIT_PROCESS]

-- | Compile match expression
-- Similar to receive but evaluates an expression instead of waiting for a message
compileMatchExpr :: Expr -> [MatchCase] -> Bytecode
compileMatchExpr matchExpr cases =
  let exprCode = compileExpr matchExpr
      -- Store match value in a temporary variable
      tmpVar = pack "_match_tmp"
      storeCode = [STORE_LOCAL tmpVar]
      -- Compile cases: each case loads the temp variable first
      casesCode = compileMatchCasesWithTmp tmpVar cases 0
  in exprCode ++ storeCode ++ casesCode

-- | Compile match cases with temporary variable
compileMatchCasesWithTmp :: Text -> [MatchCase] -> Int -> Bytecode
compileMatchCasesWithTmp _ [] _ = []
compileMatchCasesWithTmp tmpVar [MatchCase pattern action] caseNum =
  -- Last case: load tmp, pattern + action, no jump needed
  let loadCode = [LOAD_LOCAL tmpVar]
      actionCode = compileExpr action
      patternCode = compilePatternForReceive pattern (length actionCode)
  in loadCode ++ patternCode ++ actionCode
compileMatchCasesWithTmp tmpVar (MatchCase pattern action : rest) caseNum =
  let restCode = compileMatchCasesWithTmp tmpVar rest (caseNum + 1)
      loadCode = [LOAD_LOCAL tmpVar]
      actionCode = compileExpr action
      jumpCode = [JUMP (length restCode)]  -- Jump over remaining cases
      -- failOffset: skip action + JUMP + LOAD_LOCAL of next case
      failOffset = length actionCode + 1 + length loadCode
      patternCode = compilePatternForReceive pattern failOffset
  in loadCode ++ patternCode ++ actionCode ++ jumpCode ++ restCode

-- | Compile match cases (OLD VERSION with DUP - not used)
compileMatchCases :: [MatchCase] -> Int -> Bytecode
compileMatchCases [] _ = []
compileMatchCases [MatchCase pattern action] caseNum =
  -- Last case: pattern + action, no jump needed, no DUP needed
  let actionCode = compileExpr action
      patternCode = compilePatternForReceive pattern (length actionCode)
  in patternCode ++ actionCode
compileMatchCases (MatchCase pattern action : rest) caseNum =
  let restCode = compileMatchCases rest (caseNum + 1)
      actionCode = compileExpr action
      -- After action, we jump over remaining cases
      -- Cleanup: pop the duplicated value (if pattern matched, original is consumed)
      -- Actually, if pattern matched and consumed value, we don't need to pop
      -- But if pattern failed, we already jumped to DUP of next case
      -- So no cleanup needed here
      jumpCode = [JUMP (1 + length restCode)]  -- Jump over DUP + rest
      -- failOffset: skip action + JUMP + DUP, then continue with next case pattern
      failOffset = length actionCode + 1 + 1  -- action + JUMP + DUP
      patternCode = compilePatternForReceive pattern failOffset
      dupCode = [DUP]  -- Duplicate for next case
  in patternCode ++ actionCode ++ jumpCode ++ dupCode ++ restCode

-- | Simple and correct compilation of receive cases
-- Each case tries to match, if it fails, jumps to the next case
compileReceiveCasesSimple :: [ReceiveCase] -> Int -> Bytecode
compileReceiveCasesSimple [] _ = []
compileReceiveCasesSimple [Case pattern action] caseNum =
  -- Last case: pattern + action, no jump needed
  -- But pattern offsets should skip to END (after action)
  let actionCode = compileExpr action
      -- If pattern fails, skip pattern + action to reach EXIT_PROCESS
      patternCode = compilePatternForReceive pattern (length actionCode)
  in patternCode ++ actionCode
compileReceiveCasesSimple (Case pattern action : rest) caseNum =
  let -- Compile the rest first to know its size
      restCode = compileReceiveCasesSimple rest (caseNum + 1)
      -- Compile action
      actionCode = compileExpr action
      -- Calculate offset: if pattern fails, skip action + jump to reach rest
      -- Pattern is at position X, action is X+len(pattern), jump is after action
      -- To reach rest from pattern: skip len(pattern) + len(action) + 1 (jump)
      -- But jump is RELATIVE from current PC, and PC increments AFTER execute
      -- So from pattern at PC=X, to land at REST at PC=X+len(pattern)+len(action)+1:
      -- offset = len(action) + 1 - 1 (because PC increments after) = len(action)
      -- NO WAIT: jump sets PC to current+offset, THEN incrementPc
      -- So to land at X+len(pattern)+len(action)+1 from X:
      --   X + offset + 1 = X + len(pattern) + len(action) + 1
      --   offset = len(pattern) + len(action)
      -- But len(pattern) is not known yet! We're compiling it now.
      -- Actually, for simple patterns like MATCH_ATOM, len=1
      -- Let's calculate: pattern fails at some offset into pattern bytecode
      -- For MATCH_ATOM at position P in pattern:
      --   Need to skip rest of pattern + action + jump
      -- For now, assume pattern length is calculated in compilePatternForReceive
      failOffset = length actionCode + 1  -- Skip action + JUMP
      -- Compile pattern with correct fail offset
      patternCode = compilePatternForReceive pattern failOffset
      -- After action, jump over remaining cases to reach EXIT_PROCESS
      jumpCode = [JUMP (length restCode)]
  in patternCode ++ actionCode ++ jumpCode ++ restCode

-- | Compile pattern for receive block with fail offset
-- This version properly calculates nested offsets
compilePatternForReceive :: Pattern -> Int -> Bytecode
compilePatternForReceive pattern failOffset = case pattern of
  PWildcard -> [MATCH_WILDCARD]
  PVar varName -> [MATCH_VAR varName]
  PVarTyped varName _ _ -> [MATCH_VAR varName]
  PLiteral lit -> compileLiteralForReceive lit failOffset
  PAtom atomName -> [MATCH_ATOM atomName failOffset]
  PTuple patterns -> compileTupleForReceive patterns failOffset
  PArray patterns -> compileArrayForReceive patterns failOffset
  PVarargs varName -> [MATCH_VAR varName]

-- | Compile tuple pattern for receive
-- All sub-patterns jump to a single cleanup block at the end
compileTupleForReceive :: [Pattern] -> Int -> Bytecode
compileTupleForReceive patterns failOffset =
  let tupleSize = length patterns
      -- All sub-patterns jump to the same cleanup block
      compileSubPattern :: Int -> Pattern -> Bytecode
      compileSubPattern idx pat =
        let remainingPatterns = drop (idx + 1) patterns
            remainingLengths = map (\p -> length (compilePatternForReceive p 0)) remainingPatterns
            totalRemaining = sum remainingLengths
            -- Jump to cleanup block (after all sub-patterns)
            offsetToCleanup = totalRemaining
        in compilePatternForReceive pat offsetToCleanup
      
      subPatterns = concat [compileSubPattern idx p | (idx, p) <- zip [0..] patterns]
      
      -- Single cleanup block: POP all elements + JUMP to failOffset
      -- Position of this cleanup from tuple pattern start: length subPatterns
      -- JUMP is at position: length subPatterns + 1 (after POP_N)
      -- Target: failOffset (from tuple pattern start)
      -- Jump offset: failOffset - (length subPatterns + 1) - 1
      cleanupJumpOffset = failOffset - length subPatterns - 2
      cleanup = [POP_N tupleSize, JUMP cleanupJumpOffset]
      
      -- MATCH_TUPLE fails: skip sub-patterns + cleanup + reach failOffset
      tupleFailOffset = length subPatterns + length cleanup + failOffset
  in [MATCH_TUPLE tupleSize tupleFailOffset] ++ subPatterns ++ cleanup

-- | Compile array pattern for receive
compileArrayForReceive :: [Pattern] -> Int -> Bytecode
compileArrayForReceive patterns failOffset =
  let subPatterns = concatMap (\p -> compilePatternForReceive p 0) patterns
      arrayFailOffset = failOffset + length subPatterns
  in [MATCH_TUPLE (length patterns) arrayFailOffset] ++ subPatterns

-- | Compile literal pattern for receive
compileLiteralForReceive :: Literal -> Int -> Bytecode
compileLiteralForReceive lit failOffset = case lit of
  LInt n -> [MATCH_INT (fromInteger n) failOffset]
  LTypedInt _ n -> [MATCH_INT (fromInteger n) failOffset]
  LFloat f -> [PUSH_FLOAT f, MATCH_ATOM (pack $ show f) failOffset]
  LTypedFloat _ f -> [PUSH_FLOAT f, MATCH_ATOM (pack $ show f) failOffset]
  LString s -> [MATCH_STRING s failOffset]
  LBool b -> [MATCH_BOOL b failOffset]
  LNone -> [PUSH_NONE, MATCH_ATOM (pack "none") failOffset]

-- =============================================================================
-- OLD PATTERN COMPILATION (kept for backward compatibility)
-- =============================================================================

-- | Compile pattern matching to bytecode (OLD VERSION)
compilePattern :: Pattern -> Bytecode
compilePattern p = compilePatternForReceive p 0

-- | Compile a single receive case (OLD VERSION)
compileReceiveCase :: ReceiveCase -> Bytecode  
compileReceiveCase (Case pattern action) =
  let patternCode = compilePattern pattern
      actionCode = compileExpr action
  in patternCode ++ actionCode

-- =============================================================================
-- HELPER FUNCTIONS (OLD VERSIONS)
-- =============================================================================

-- | Compile literal pattern matching (OLD VERSION)
compileLiteralPattern :: Literal -> Bytecode
compileLiteralPattern lit = compileLiteralForReceive lit 0

-- | Compile tuple pattern matching (OLD VERSION)
compileTuplePattern :: [Pattern] -> Bytecode
compileTuplePattern patterns = compileTupleForReceive patterns 0

-- | Compile array pattern matching (OLD VERSION)
compileArrayPattern :: [Pattern] -> Bytecode
compileArrayPattern patterns =
  let subPatterns = concatMap compilePattern patterns
  in [MATCH_TUPLE (length patterns) 999] ++ subPatterns

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
  LBool b -> [PUSH_BOOL b]
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

-- | Compile unary operations
compileUnaryOp :: UnaryOp -> Expr -> Bytecode
compileUnaryOp op expr =
  let exprCode = compileExpr expr
      opCode = case op of
        UNot -> [LOGIC_NOT]
        UNeg -> [NEGATE]
        UPlus -> []  -- Unary plus is a no-op
  in exprCode ++ opCode

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
compileCall funcName args
  | funcName == pack "print" = concatMap compileExpr args ++ [PRINT]
  | otherwise = concatMap compileExpr args ++ [CALL funcName]

-- | Compile process spawning
compileSpawn :: Text -> [Expr] -> Bytecode
compileSpawn procNameArg args = concatMap compileExpr args ++ [CREATE_INSTANCE procNameArg (length args)]

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
compileAssign varName expr
  | varName == pack "state" = compileExpr expr ++ [SET_STATE]  -- Special handling for state
  | otherwise = compileExpr expr ++ [STORE_LOCAL varName]

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
