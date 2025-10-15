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
  deriving (Show, Eq)

-- | Bytecode instructions for the Nexus VM
data Instruction
  = -- Stack operations
    PUSH_INT Integer
  | PUSH_STRING Text
  | PUSH_ATOM Text
  | PUSH_TUPLE Int -- Number of elements to pop and create tuple
  | -- Variable operations
    LOAD_VAR Text
  | STORE_VAR Text
  | -- Arithmetic operations
    ADD
  | SUB
  | MUL
  | DIV
  | -- Actor model operations
    SPAWN Text [Value] -- Process name and arguments
  | SEND -- Expects receiver and message on stack
  | RECEIVE [ReceiveCase] -- Pattern matching cases
  | SELF -- Push current process PID
  | -- Control flow
    JUMP Int -- Jump to instruction offset
  | JUMP_IF_FALSE Int -- Conditional jump
  | CALL Text -- Call function/process
  | RETURN
  | HALT
  deriving (Show, Eq)

-- | A sequence of bytecode instructions
type Bytecode = [Instruction]

-- | Compile an expression to bytecode
compileExpr :: Expr -> Bytecode
compileExpr expr = case expr of
  -- Literals
  ELiteral (LInt n) -> [PUSH_INT n]
  ELiteral (LString s) -> [PUSH_STRING s]
  -- Variables
  EVar varName -> [LOAD_VAR varName]
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
    in leftCode ++ rightCode ++ opCode
  -- Spawn process
  ESpawn procName args ->
    let argsCode = concatMap compileExpr args
    in argsCode ++ [SPAWN procName []]
  -- Send message
  ESend receiver message ->
    let receiverCode = compileExpr receiver
        messageCode = compileExpr message
    in receiverCode ++ messageCode ++ [SEND]
  -- Receive block
  EReceive cases -> [RECEIVE cases]
  -- Block with statements
  EBlock statements finalExpr ->
    let statementsCode = concatMap compileStmt statements
        finalCode = compileExpr finalExpr
    in statementsCode ++ finalCode

-- | Compile a statement to bytecode
compileStmt :: Stmt -> Bytecode
compileStmt stmt = case stmt of
  SLet varName expr -> compileExpr expr ++ [STORE_VAR varName]
  SExpr expr -> compileExpr expr

-- | Compile a full program to bytecode
compileProgram :: Program -> Bytecode
compileProgram (Program definitions) = 
  concatMap compileDefinition definitions ++ [HALT]

-- | Compile a definition to bytecode
compileDefinition :: Definition -> Bytecode
compileDefinition (ProcDef procName params procBody) =
  -- Pour l'instant, on génère un code simple
  -- Une vraie implémentation nécessiterait une gestion plus complexe des processus
  let bodyCode = compileProcBody procBody
  in bodyCode

-- | Compile a process body to bytecode
compileProcBody :: ProcBody -> Bytecode
compileProcBody (ProcBody maybeState receiveCases) =
  let stateCode = case maybeState of
        Nothing -> []
        Just stateExpr -> compileExpr stateExpr ++ [STORE_VAR (pack "state")]
      receiveCode = if null receiveCases 
                   then []
                   else [RECEIVE receiveCases]
  in stateCode ++ receiveCode