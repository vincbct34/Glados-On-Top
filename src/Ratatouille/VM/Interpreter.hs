{-
-- EPITECH PROJECT, 2025
-- glados-vm
-- File description:
-- Bytecode interpreter
-}

module Ratatouille.VM.Interpreter 
  ( executeInstruction
  , executeProcessBytecode
  , traceInstruction
  , executeBytecode
  , registerLabels
  , executeLoop
  , valueToString
  ) where

import Ratatouille.Bytecode.Types
import Ratatouille.VM.VM
import Ratatouille.VM.Runtime
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent.STM (atomically, readTVar)
import qualified Data.Text as T
import System.IO (hFlush, stdout, isEOF)
import qualified Data.Map as Map

-- | Execute a single instruction
executeInstruction :: Instruction -> VM ()
executeInstruction instr = do
  traceInstruction instr
  case instr of
    PUSH_INT n -> pushStack (VInt n)
    PUSH_STRING s -> pushStack (VString s)
    PUSH_ATOM a -> pushStack (VAtom a)
    PUSH_UNIT -> pushStack VUnit
    PUSH_NONE -> pushStack VNone
    PUSH_BOOL b -> pushStack (VBool b)

    -- Maybe/Either operations
    PUSH_JUST -> do
      val <- popStack
      pushStack (VJust val)
    
    PUSH_LEFT -> do
      val <- popStack
      pushStack (VLeft val)
    
    PUSH_RIGHT -> do
      val <- popStack
      pushStack (VRight val)

    PUSH_TUPLE n -> do
      elements <- popStackN n
      pushStack (VTuple $ reverse elements)
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

    -- Increment/Decrement operations
    INC_VAR name -> do
      val <- loadLocal name
      case val of
        VInt n -> do
          let newVal = VInt (n + 1)
          storeLocal name newVal
          pushStack newVal
        _ -> throwError (TypeError "INC_VAR requires integer")

    DEC_VAR name -> do
      val <- loadLocal name
      case val of
        VInt n -> do
          let newVal = VInt (n - 1)
          storeLocal name newVal
          pushStack newVal
        _ -> throwError (TypeError "DEC_VAR requires integer")

    INC_VAR_POST name -> do
      val <- loadLocal name
      case val of
        VInt n -> do
          let newVal = VInt (n + 1)
          storeLocal name newVal
          pushStack val  -- Push old value
        _ -> throwError (TypeError "INC_VAR_POST requires integer")

    DEC_VAR_POST name -> do
      val <- loadLocal name
      case val of
        VInt n -> do
          let newVal = VInt (n - 1)
          storeLocal name newVal
          pushStack val  -- Push old value
        _ -> throwError (TypeError "DEC_VAR_POST requires integer")

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
    CMP_EQ -> comparisonOp (==) "CMP_EQ"
    CMP_NEQ -> comparisonOp (/=) "CMP_NEQ"
    CMP_LT -> intComparisonOp (<) "CMP_LT"
    CMP_GT -> intComparisonOp (>) "CMP_GT"
    CMP_LTE -> intComparisonOp (<=) "CMP_LTE"
    CMP_GTE -> intComparisonOp (>=) "CMP_GTE"
    LOGIC_AND -> do
      b <- popStack >>= toBool
      a <- popStack >>= toBool
      pushStack (VBool (a && b))
    LOGIC_OR -> do
      b <- popStack >>= toBool
      a <- popStack >>= toBool
      pushStack (VBool (a || b))
    GET_FIELD name -> do
      val <- popStack
      case val of
        VTuple fields -> do
          case readMaybe (T.unpack name) of
            Just idx | idx >= 0 && idx < length fields ->
              pushStack (fields !! idx)
            _ -> throwError $ RuntimeError $ "Invalid field: " ++ T.unpack name
        _ -> throwError $ TypeError $ "GET_FIELD requires tuple, got " ++ show val
    DEFINE_PROCESS name params body -> do
      let pdef = ProcessDef name params body
      defineProcess pdef
    CREATE_INSTANCE name -> do
      pid <- createProcessInstance name
      pushStack (VPid $ fromPid pid)
    SEND -> do
      msg <- popStack
      receiver <- popStack >>= toPid
      -- Create a tuple (message, sender) as expected by receive patterns
      sender <- getCurrentPid
      let tupleMsg = VTuple [msg, VPid (fromPid sender)]
      sendMessage receiver tupleMsg
      debugPutStrLn $ "Message tuple '" ++ show tupleMsg ++ "' sent to process " ++ show receiver
      -- Execute the receiving process synchronously
      executeReceivingProcess receiver
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
    MATCH_ATOM atom offset -> do
      val <- peekStack
      case val of
        VAtom a | a == atom -> do
          _ <- popStack
          return ()
        _ -> jump offset
    MATCH_VAR name -> do
      val <- popStack
      storeLocal name val
    MATCH_TUPLE size offset -> do
      val <- peekStack
      case val of
        VTuple elements | length elements == size -> do
          _ <- popStack
          mapM_ pushStack (reverse elements)
        _ -> jump offset
    MATCH_WILDCARD -> do
      _ <- popStack
      return ()
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
      return ()
    PRINT -> do
      val <- popStack
      liftIO $ putStrLn $ valueToString val
      pushStack VUnit
    HALT -> throwError $ RuntimeError "HALT instruction executed"
    
    -- Monadic operations
    MAYBE_BIND funcName -> do
      mVal <- popStack
      case mVal of
        VJust val -> do
          -- Check if label exists
          labels <- gets vmLabels
          case Map.lookup funcName labels of
            Just pc -> do
              pushStack val  -- Push the unwrapped value
              jumpTo pc
            Nothing -> throwError $ RuntimeError $ "Label " ++ T.unpack funcName ++ " not found"
        VNone -> pushStack VNone  -- Propagate None
        _ -> throwError $ TypeError "MAYBE_BIND requires Maybe value"
    
    EITHER_BIND funcName -> do
      eVal <- popStack
      case eVal of
        VRight val -> do
          -- Check if label exists
          labels <- gets vmLabels
          case Map.lookup funcName labels of
            Just pc -> do
              pushStack val  -- Push the unwrapped value
              jumpTo pc
            Nothing -> throwError $ RuntimeError $ "Label " ++ T.unpack funcName ++ " not found"
        VLeft err -> pushStack (VLeft err)  -- Propagate Left
        _ -> throwError $ TypeError "EITHER_BIND requires Either value"
    
    -- Float operations (TODO: implement proper float support in VM)
    PUSH_FLOAT _f -> throwError $ RuntimeError "PUSH_FLOAT not yet implemented in VM"
    
    -- Array operations (TODO: implement array support in VM)
    PUSH_ARRAY _n -> throwError $ RuntimeError "PUSH_ARRAY not yet implemented in VM"
    INDEX -> throwError $ RuntimeError "INDEX not yet implemented in VM"
    ARRAY_LENGTH -> throwError $ RuntimeError "ARRAY_LENGTH not yet implemented in VM"
    
    -- Cast operations (TODO: implement type casting in VM)
    STATIC_CAST _t -> throwError $ RuntimeError "STATIC_CAST not yet implemented in VM"
    REINTERPRET_CAST _t -> throwError $ RuntimeError "REINTERPRET_CAST not yet implemented in VM"
    CONST_CAST -> throwError $ RuntimeError "CONST_CAST not yet implemented in VM"

-- | Helper for binary operations
binaryOp :: (Integer -> Integer -> Integer) -> String -> VM ()
binaryOp op _name = do
  b <- popStack >>= toInt
  a <- popStack >>= toInt
  pushStack (VInt (op a b))

-- | Helper for comparison operations
comparisonOp :: (Value -> Value -> Bool) -> String -> VM ()
comparisonOp cmp _name = do
  b <- popStack
  a <- popStack
  pushStack (VBool (cmp a b))

-- | Helper for integer comparison operations
intComparisonOp :: (Integer -> Integer -> Bool) -> String -> VM ()
intComparisonOp cmp _name = do
  b <- popStack >>= toInt
  a <- popStack >>= toInt
  pushStack (VBool (cmp a b))

-- | Read a Maybe Integer from String
readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing

-- | Convert a Value to a printable String
valueToString :: Value -> String
valueToString val = case val of
  VInt n -> show n
  VFloat f -> show f
  VString s -> T.unpack s
  VAtom a -> ":" ++ T.unpack a
  VBool b -> if b then "true" else "false"
  VUnit -> "()"
  VNone -> "none"
  VPid n -> "<pid:" ++ show n ++ ">"
  VTuple elements -> "(" ++ intercalate ", " (map valueToString elements) ++ ")"
  VArray elements -> "[" ++ intercalate ", " (map valueToString elements) ++ "]"
  VJust inner -> "Just " ++ valueToString inner
  VLeft inner -> "Left " ++ valueToString inner
  VRight inner -> "Right " ++ valueToString inner
  where
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | Execute bytecode program
executeBytecode :: Bytecode -> VM Value
executeBytecode code = do
  registerLabels code 0
  modify $ \s -> s { vmBytecode = code, vmPc = 0 }
  executeLoop
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
  if pc >= length bytecode
    then return ()
    else do
      atBreakpoint <- checkBreakpoint
      when atBreakpoint $ do
        debugPutStrLn $ "Breakpoint hit at PC=" ++ show pc
      let instr = bytecode !! pc
      case instr of
        HALT -> return ()
        RETURN -> return ()
        EXIT_PROCESS -> return ()
        _ -> do
          executeInstruction instr
          incrementPc
          executeLoop

startREPL :: VM ()
startREPL = do
  liftIO $ putStr "> " >> hFlush stdout
  eof <- liftIO isEOF
  if eof
    then liftIO $ putStrLn ""
    else do
      line <- liftIO getLine
      let cmd = T.strip (T.pack line)
      case cmd of
        c | c == T.pack ":quit" -> liftIO $ putStrLn "Bye."
        c | c == T.pack ":stack" -> do
          st <- gets vmStack
          liftIO $ putStrLn $ "Stack: " ++ show (map valueToString st)
          startREPL
        c | c == T.pack ":state" -> do
          s <- getProcessState
          liftIO $ putStrLn $ "Process state: " ++ valueToString s
          startREPL
        c | c == T.empty -> startREPL
        _ -> do
          -- Default behaviour: push the entered text as a string value onto the VM stack
          pushStack (VString (T.pack line))
          liftIO $ putStrLn "(pushed string onto stack)"
          startREPL

-- | Execute process bytecode 
executeProcessBytecode :: Bytecode -> VM Value
executeProcessBytecode code = do
  modify $ \s -> s { vmBytecode = code, vmPc = 0 }
  registerLabels code 0
  executeLoop
  return VUnit

-- | Execute a receiving process (synchronous for now)
executeReceivingProcess :: Pid -> VM ()
executeReceivingProcess pid = do
  -- Print debug message
  debugPutStrLn $ "Executing process " ++ show pid
  processesVar <- gets vmProcesses
  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup pid processes
  case maybeProcess of
    Nothing -> debugPutStrLn $ "Process " ++ show pid ++ " not found"
    Just process -> do
      -- Save current state
      currentState <- get
      debugPutStrLn $ "Switching to process " ++ show pid
      -- Set up process state
      put $ currentState 
        { vmStack = processStack process
        , vmLocals = processLocals process
        , vmBytecode = processBytecode process
        , vmPc = processPc process
        , vmCurrentPid = Just pid
        }
      -- Execute the process
      bytecode <- gets vmBytecode
      debugPutStrLn $ "Registering labels for process " ++ show pid
      registerLabels bytecode 0
      debugPutStrLn $ "Starting execution loop for process " ++ show pid
      executeLoop
      debugPutStrLn $ "Process " ++ show pid ++ " finished execution"
      -- Restore original state
      put currentState
