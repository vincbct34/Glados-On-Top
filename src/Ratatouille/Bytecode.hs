{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Bytecode definitions for Ratatouille VM
-}

module Ratatouille.Bytecode
  ( Instruction (..),
    Bytecode,
    Value (..),
    compileExpr,
    compileProgram,
    compilePattern,
    compileStmt,
    compileDefinition,
    compileReceiveBlock,
  )
where

import Data.Text (Text, pack)
import Ratatouille.AST

-- | Values that can be stored and manipulated by the VM
data Value
  = VInt Integer
  | VString Text
  | VAtom Text
  | VTuple [Value]
  | VPid Integer -- Process ID
  | VUnit         -- Unit value for void operations
  | VNone         -- None value (null/absence)
  | VBool Bool    -- Boolean value
  deriving (Show, Eq, Read)

-- | Bytecode instructions for the Nexus VM
data Instruction
  = -- Stack operations
    PUSH_INT Integer
  | PUSH_STRING Text
  | PUSH_ATOM Text
  | PUSH_TUPLE Int -- Number of elements to pop and create tuple
  | PUSH_UNIT     -- Push unit value
  -- Variable operations (global scope)
  | LOAD_VAR Text
  | STORE_VAR Text
  -- Local variable operations (process-scoped)
  | LOAD_LOCAL Text
  | STORE_LOCAL Text
  -- Process state operations
  | INIT_STATE     -- Initialize process state from stack top
  | GET_STATE      -- Push current process state to stack
  | SET_STATE      -- Set process state from stack top
  -- Arithmetic operations
  | ADD
  | SUB
  | MUL
  | DIV
  | CONCAT       -- String concatenation
  -- Comparison operations
  | CMP_EQ       -- Equality
  | CMP_NEQ      -- Not equal
  | CMP_LT       -- Less than
  | CMP_GT       -- Greater than
  | CMP_LTE      -- Less than or equal
  | CMP_GTE      -- Greater than or equal
  -- Logical operations
  | LOGIC_AND    -- Logical and
  | LOGIC_OR     -- Logical or
  -- Value operations
  | PUSH_NONE    -- Push none value
  | PUSH_BOOL Bool  -- Push boolean value
  | GET_FIELD Text  -- Get field from tuple/record
  -- Actor model operations
  | DEFINE_PROCESS Text [Text] Bytecode -- Name, params, body bytecode
  | CREATE_INSTANCE Text               -- Create process instance, push PID
  | SEND                               -- Send message (receiver, message on stack)
  | WAIT_MESSAGE                       -- Wait for next message
  -- Pattern matching operations
  | MATCH_ATOM Text Int                -- Match atom, jump offset if no match
  | MATCH_VAR Text                     -- Match and bind variable
  | MATCH_TUPLE Int Int                -- Match tuple of size N, jump offset if no match
  | MATCH_WILDCARD                     -- Match anything (always succeeds)
  -- Process control
  | PROCESS_LOOP                       -- Main process message loop
  | SELF                               -- Push current process PID
  | EXIT_PROCESS                       -- Terminate current process
  -- Control flow
  | JUMP Int                           -- Unconditional jump
  | JUMP_IF_FALSE Int                  -- Conditional jump
  | LABEL Text                         -- Jump target label
  | CALL Text                          -- Call function/process
  | RETURN
  | HALT
  -- Built-in functions
  | PRINT                              -- Print top of stack to stdout
  deriving (Show, Eq, Read)

-- | A sequence of bytecode instructions
type Bytecode = [Instruction]

-- | Compile an expression to bytecode
compileExpr :: Expr -> Bytecode
compileExpr expr = case expr of
  -- Literals
  ELiteral (LInt n) -> [PUSH_INT n]
  ELiteral (LString s) -> [PUSH_STRING s]
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
  
  -- Function/procedure call
  ECall funcName args ->
    -- Handle built-in functions
    if funcName == pack "print"
      then case args of
        [arg] -> compileExpr arg ++ [PRINT]
        _ -> error "print() expects exactly one argument"
      else let argsCode = concatMap compileExpr args
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

-- | Compile a statement to bytecode
compileStmt :: Stmt -> Bytecode
compileStmt stmt = case stmt of
  -- Let bindings use local variables in process context
  SLet varName expr -> compileExpr expr ++ [STORE_LOCAL varName]
  
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
  PWildcard -> [MATCH_WILDCARD]

  PVar varName -> [MATCH_VAR varName]

  PLiteral (LInt n) -> [PUSH_INT n, MATCH_ATOM (pack $ show n) 2]  -- Jump 2 if no match
  PLiteral (LString s) -> [PUSH_STRING s, MATCH_ATOM s 2]
  PLiteral LNone -> [PUSH_NONE, MATCH_ATOM (pack "none") 2]

  PAtom atomName -> [MATCH_ATOM atomName 2]  -- Jump 2 instructions if no match

  PTuple patterns ->
    let numElements = length patterns
        subPatterns = concatMap compilePattern patterns
        jumpOffset = length subPatterns + 1
    in [MATCH_TUPLE numElements jumpOffset] ++ subPatterns

  -- Variadic patterns: capture remaining elements
  -- For now, compile as a simple variable that captures all remaining
  PVarargs varName -> [MATCH_VAR varName]  -- Simplified implementation