{-
-- EPITECH PROJECT, 2025
-- glados-vm
-- File description:
-- Bytecode interpreter
-}

module Ratatouille.VM.Interpreter where

import Ratatouille.Bytecode
import Ratatouille.VM.VM
import Ratatouille.VM.Runtime
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T

-- | Execute a single instruction
executeInstruction :: Instruction -> VM ()
executeInstruction instr = do
  traceInstruction instr
  case instr of
    -- Stack operations
    PUSH_INT n -> pushStack (VInt n)
    PUSH_STRING s -> pushStack (VString s)
    PUSH_ATOM a -> pushStack (VAtom a)
    PUSH_UNIT -> pushStack VUnit
    PUSH_NONE -> pushStack VNone
    PUSH_BOOL b -> pushStack (VBool b)

    PUSH_TUPLE n -> do
      elements <- popStackN n
      pushStack (VTuple $ reverse elements)

    -- Variable operations
    LOAD_VAR name -> do
      val <- loadGlobal name
      pushStack val

    STORE_VAR name -> do
      val <- popStack
      storeGlobal name val

    LOAD_LOCAL name -> do
      val <- loadLocal name
      pushStack val

    STORE_LOCAL name -> do
      val <- popStack
      storeLocal name val

    -- Process state operations
    INIT_STATE -> do
      val <- popStack
      setProcessState val

    GET_STATE -> do
      val <- getProcessState
      pushStack val

    SET_STATE -> do
      val <- popStack
      setProcessState val

    -- Arithmetic operations
    ADD -> binaryOp (+) "ADD"
    SUB -> binaryOp (-) "SUB"
    MUL -> binaryOp (*) "MUL"
    DIV -> do
      b <- popStack >>= toInt
      a <- popStack >>= toInt
      when (b == 0) $ throwError DivisionByZero
      pushStack (VInt (a `div` b))

    CONCAT -> do
      b <- popStack >>= toString
      a <- popStack >>= toString
      pushStack (VString (a <> b))

    -- Comparison operations
    CMP_EQ -> comparisonOp (==) "CMP_EQ"
    CMP_NEQ -> comparisonOp (/=) "CMP_NEQ"
    CMP_LT -> intComparisonOp (<) "CMP_LT"
    CMP_GT -> intComparisonOp (>) "CMP_GT"
    CMP_LTE -> intComparisonOp (<=) "CMP_LTE"
    CMP_GTE -> intComparisonOp (>=) "CMP_GTE"

    -- Logical operations
    LOGIC_AND -> do
      b <- popStack >>= toBool
      a <- popStack >>= toBool
      pushStack (VBool (a && b))

    LOGIC_OR -> do
      b <- popStack >>= toBool
      a <- popStack >>= toBool
      pushStack (VBool (a || b))

    -- Value operations
    GET_FIELD name -> do
      val <- popStack
      case val of
        VTuple fields -> do
          -- Simple field access by index (field names as numbers)
          case readMaybe (T.unpack name) of
            Just idx | idx >= 0 && idx < length fields ->
              pushStack (fields !! idx)
            _ -> throwError $ RuntimeError $ "Invalid field: " ++ T.unpack name
        _ -> throwError $ TypeError $ "GET_FIELD requires tuple, got " ++ show val

    -- Actor model operations
    DEFINE_PROCESS name params body -> do
      let pdef = ProcessDef name params body
      defineProcess pdef

    CREATE_INSTANCE name -> do
      pid <- createProcessInstance name
      pushStack (VPid $ fromPid pid)

    SEND -> do
      msg <- popStack
      receiver <- popStack >>= toPid
      sendMessage receiver msg

    WAIT_MESSAGE -> do
      msg <- waitMessage
      pushStack msg

    SELF -> do
      pid <- getCurrentPid
      pushStack (VPid $ fromPid pid)

    EXIT_PROCESS -> do
      exitCurrentProcess

    PROCESS_LOOP -> do
      processMessageLoop

    -- Pattern matching operations
    MATCH_ATOM atom offset -> do
      val <- peekStack
      case val of
        VAtom a | a == atom -> do
          _ <- popStack  -- Consume the matched value
          return ()
        _ -> jump offset

    MATCH_VAR name -> do
      val <- popStack
      storeLocal name val

    MATCH_TUPLE size offset -> do
      val <- peekStack
      case val of
        VTuple elements | length elements == size -> do
          _ <- popStack  -- Consume the tuple
          -- Push elements in reverse order so they can be matched left-to-right
          mapM_ pushStack (reverse elements)
        _ -> jump offset

    MATCH_WILDCARD -> do
      _ <- popStack  -- Consume the value without binding
      return ()

    -- Control flow
    JUMP offset -> jump offset

    JUMP_IF_FALSE offset -> do
      cond <- popStack >>= toBool
      unless cond $ jump offset

    LABEL name -> do
      pc <- getPc
      registerLabel name pc

    CALL name -> do
      pc <- findLabel name
      jumpTo pc

    RETURN -> do
      -- Pop return address and jump back
      -- For now, simplified: just halt or return control
      return ()

    HALT -> throwError $ RuntimeError "HALT instruction executed"

-- | Helper for binary arithmetic operations
binaryOp :: (Integer -> Integer -> Integer) -> String -> VM ()
binaryOp op name = do
  b <- popStack >>= toInt
  a <- popStack >>= toInt
  pushStack (VInt (op a b))

-- | Helper for comparison operations
comparisonOp :: (Value -> Value -> Bool) -> String -> VM ()
comparisonOp op name = do
  b <- popStack
  a <- popStack
  pushStack (VBool (op a b))

-- | Helper for integer comparison operations
intComparisonOp :: (Integer -> Integer -> Bool) -> String -> VM ()
intComparisonOp op name = do
  b <- popStack >>= toInt
  a <- popStack >>= toInt
  pushStack (VBool (op a b))

-- | Read a Maybe Integer from String
readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

-- | Execute bytecode program
executeBytecode :: Bytecode -> VM Value
executeBytecode code = do
  -- First pass: register all labels
  registerLabels code 0

  -- Execute instructions
  modify $ \s -> s { vmBytecode = code, vmPc = 0 }
  executeLoop

  -- Return top of stack or VUnit if empty
  stack <- gets vmStack
  case stack of
    (x:_) -> return x
    [] -> return VUnit

-- | Register all labels in the bytecode
registerLabels :: Bytecode -> Int -> VM ()
registerLabels [] _ = return ()
registerLabels (LABEL name : rest) pc = do
  registerLabel name pc
  registerLabels rest (pc + 1)
registerLabels (_ : rest) pc = registerLabels rest (pc + 1)

-- | Main execution loop
executeLoop :: VM ()
executeLoop = do
  pc <- getPc
  bytecode <- gets vmBytecode

  -- Check if we've reached the end
  when (pc >= length bytecode) $
    return ()

  -- Check for breakpoint
  atBreakpoint <- checkBreakpoint
  when atBreakpoint $ do
    liftIO $ putStrLn $ "Breakpoint hit at PC=" ++ show pc
    -- In a full implementation, we'd enter debug mode here

  -- Get current instruction
  let instr = bytecode !! pc

  -- Execute instruction
  case instr of
    HALT -> return ()
    RETURN -> return ()
    EXIT_PROCESS -> return ()
    _ -> do
      executeInstruction instr
      incrementPc
      executeLoop
