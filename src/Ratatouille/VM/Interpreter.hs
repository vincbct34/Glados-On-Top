{-
-- EPITECH PROJECT, 2025
-- glados-vm
-- File description:
-- Bytecode interpreter
-}

module Ratatouille.VM.Interpreter
  ( executeInstruction,
    executeProcessBytecode,
    traceInstruction,
    executeBytecode,
    registerLabels,
    executeLoop,
    valueToString,
  )
where

import Control.Concurrent.STM (atomically, modifyTVar, readTVar)
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Ratatouille.Bytecode.Types
import Ratatouille.VM.Runtime
import Ratatouille.VM.VM
import System.IO (hFlush, isEOF, stdout)

-- | Execute a single instruction
executeInstruction :: Instruction -> VM ()
executeInstruction instr =
  traceInstruction instr
    >> case instr of
      PUSH_INT n -> pushStack (VInt n)
      PUSH_STRING s -> pushStack (VString s)
      PUSH_ATOM a -> pushStack (VAtom a)
      PUSH_UNIT -> pushStack VUnit
      PUSH_NONE -> pushStack VNone
      PUSH_BOOL b -> pushStack (VBool b)
      POP_N n ->
        debugPutStrLn ("[POP_N] Popping " ++ show n ++ " elements from stack")
          >> replicateM_ n (void popStack)
      DUP -> do
        val <- peekStack
        pushStack val

      -- Maybe/Either operations
      PUSH_JUST ->
        popStack >>= pushStack . VJust
      PUSH_LEFT ->
        popStack >>= pushStack . VLeft
      PUSH_RIGHT ->
        popStack >>= pushStack . VRight
      PUSH_TUPLE n -> do
        elements <- popStackN n
        pushStack (VTuple $ reverse elements)
      LOAD_VAR name ->
        loadGlobal name >>= pushStack
      STORE_VAR name ->
        popStack >>= storeGlobal name
      LOAD_LOCAL name ->
        loadLocal name >>= pushStack
      STORE_LOCAL name ->
        popStack >>= storeLocal name
      -- Increment/Decrement operations
      INC_VAR name -> do
        val <- loadLocal name
        case val of
          VInt n ->
            let newVal = VInt (n + 1)
             in storeLocal name newVal >> pushStack newVal
          _ -> throwError (TypeError "INC_VAR requires integer")
      DEC_VAR name -> do
        val <- loadLocal name
        case val of
          VInt n ->
            let newVal = VInt (n - 1)
             in storeLocal name newVal >> pushStack newVal
          _ -> throwError (TypeError "DEC_VAR requires integer")
      INC_VAR_POST name -> do
        val <- loadLocal name
        case val of
          VInt n ->
            let newVal = VInt (n + 1)
             in storeLocal name newVal >> pushStack val
          _ -> throwError (TypeError "INC_VAR_POST requires integer")
      DEC_VAR_POST name -> do
        val <- loadLocal name
        case val of
          VInt n ->
            let newVal = VInt (n - 1)
             in storeLocal name newVal >> pushStack val
          _ -> throwError (TypeError "DEC_VAR_POST requires integer")

      -- Process state operations
      INIT_STATE ->
        popStack >>= setProcessState
      GET_STATE ->
        getProcessState >>= pushStack
      SET_STATE ->
        popStack >>= setProcessState
      ADD -> binaryOp (+) "ADD"
      SUB -> binaryOp (-) "SUB"
      MUL -> binaryOp (*) "MUL"
      DIV -> do
        b <- popStack
        a <- popStack
        case (a, b) of
          (VInt _, VInt 0) -> throwError DivisionByZero
          (VInt x, VInt y) -> pushStack (VInt (x `div` y))
          (VFloat _, VFloat 0) -> throwError DivisionByZero
          (VFloat x, VFloat y) -> pushStack (VFloat (x / y))
          (VInt _, VFloat 0) -> throwError DivisionByZero
          (VInt x, VFloat y) -> pushStack (VFloat (fromInteger x / y))
          (VFloat _, VInt 0) -> throwError DivisionByZero
          (VFloat x, VInt y) -> pushStack (VFloat (x / fromInteger y))
          _ ->
            throwError $
              TypeError $
                "DIV requires numeric values, got "
                  ++ show a
                  ++ " and "
                  ++ show b
      CONCAT -> do
        b <- popStack
        a <- popStack
        pushStack (VString (T.pack (valueToString a <> valueToString b)))
      CMP_EQ -> comparisonOp (==) "CMP_EQ"
      CMP_NEQ -> comparisonOp (/=) "CMP_NEQ"
      CMP_LT -> intComparisonOp (<) "CMP_LT"
      CMP_GT -> intComparisonOp (>) "CMP_GT"
      CMP_LTE -> intComparisonOp (<=) "CMP_LTE"
      CMP_GTE -> intComparisonOp (>=) "CMP_GTE"
      LOGIC_AND ->
        (&&) <$> (popStack >>= toBool) <*> (popStack >>= toBool)
          >>= pushStack . VBool
      LOGIC_OR ->
        (||) <$> (popStack >>= toBool) <*> (popStack >>= toBool)
          >>= pushStack . VBool
      LOGIC_NOT ->
        popStack >>= toBool >>= pushStack . VBool . not
      NEGATE -> do
        val <- popStack
        case val of
          VInt n -> pushStack (VInt (negate n))
          VFloat f -> pushStack (VFloat (negate f))
          _ -> throwError $ TypeError $ "NEGATE requires numeric value, got " ++ show val
      GET_FIELD name -> do
        val <- popStack
        case val of
          VTuple fields ->
            case readMaybe (T.unpack name) of
              Just idx
                | idx >= 0 && idx < length fields ->
                    pushStack (fields !! idx)
              _ ->
                throwError $
                  RuntimeError $
                    "Invalid field: " ++ T.unpack name
          _ -> throwError $ TypeError $ "GET_FIELD requires tuple, got " ++ show val
      DEFINE_PROCESS name params body ->
        let pdef = ProcessDef name params body
         in defineProcess pdef
      CREATE_INSTANCE name argCount -> do
        -- Pop arguments from stack
        args <- popStackN argCount
        pid <- createProcessInstance name (reverse args) -- Reverse to get correct order
        pushStack (VPid $ fromPid pid)
      SEND -> do
        msg <- popStack
        receiver <- popStack >>= toPid
        -- Send the message as-is (sender is already included in message tuple if needed)
        sender <- getCurrentPid
        sendMessage receiver msg
        debugPutStrLn $ "Message '" ++ show msg ++ "' sent from " ++ show sender ++ " to process " ++ show receiver
        -- Execute the receiving process synchronously ONLY if it's not the main process
        -- The main process (Pid 0) will handle its messages via its own receive block
        when (receiver /= Pid 0) $ executeReceivingProcess receiver
      WAIT_MESSAGE -> do
        (msg, senderPid) <- waitMessageWithSender
        debugPutStrLn $ "[WAIT_MESSAGE] Received message: " ++ show msg ++ " from pid " ++ show senderPid
        -- Automatically store the sender in the local variable "sender"
        storeLocal (T.pack "sender") (VPid $ fromPid senderPid)
        pushStack msg
      SELF -> do
        pid <- getCurrentPid
        pushStack (VPid $ fromPid pid)
      EXIT_PROCESS ->
        return ()
      PROCESS_LOOP ->
        processMessageLoop
      MATCH_ATOM atom offset -> do
        val <- peekStack
        debugPutStrLn $ "[MATCH_ATOM] Trying to match atom: " ++ show atom ++ " against value: " ++ show val ++ " (fail offset: " ++ show offset ++ ")"
        case val of
          VAtom a
            | a == atom ->
                debugPutStrLn "[MATCH_ATOM] MATCH SUCCESS: atom matches"
                  >> void popStack
          _ ->
            debugPutStrLn
              ( "[MATCH_ATOM] MATCH FAILED: jumping to offset "
                  ++ show offset
              )
              >> jump offset
      MATCH_VAR name ->
        popStack >>= storeLocal name
      MATCH_TUPLE size offset -> do
        val <- peekStack
        debugPutStrLn $ "[MATCH_TUPLE] Trying to match tuple of size " ++ show size ++ " against value: " ++ show val ++ " (fail offset: " ++ show offset ++ ")"
        case val of
          VTuple elements
            | length elements == size ->
                debugPutStrLn
                  ( "[MATCH_TUPLE] MATCH SUCCESS: tuple size matches, "
                      ++ "pushing "
                      ++ show (length elements)
                      ++ " elements"
                  )
                  >> void popStack
                  >> mapM_ pushStack (reverse elements)
          _ ->
            debugPutStrLn
              ( "[MATCH_TUPLE] MATCH FAILED: expected size "
                  ++ show size
                  ++ " but got "
                  ++ show (case val of VTuple e -> length e; _ -> 0)
                  ++ ", jumping to offset "
                  ++ show offset
              )
              >> jump offset
      MATCH_WILDCARD ->
        void popStack
      MATCH_INT expected offset -> do
        val <- peekStack
        case val of
          VInt n
            | n == toInteger expected ->
                void popStack
          _ -> jump offset
      MATCH_BOOL expected offset -> do
        val <- peekStack
        case val of
          VBool b
            | b == expected ->
                void popStack
          _ -> jump offset
      MATCH_STRING expected offset -> do
        val <- peekStack
        case val of
          VString s
            | s == expected ->
                void popStack
          _ -> jump offset
      JUMP offset -> jump offset
      JUMP_IF_FALSE offset -> do
        cond <- popStack >>= toBool
        unless cond $ jump offset
      LABEL name ->
        getPc >>= registerLabel name
      CALL name ->
        findLabel name >>= jumpTo
      RETURN ->
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
              Just pc ->
                pushStack val >> jumpTo pc
              Nothing ->
                throwError $
                  RuntimeError $
                    "Label " ++ T.unpack funcName ++ " not found"
          VNone -> pushStack VNone
          _ -> throwError $ TypeError "MAYBE_BIND requires Maybe value"
      EITHER_BIND funcName -> do
        eVal <- popStack
        case eVal of
          VRight val -> do
            labels <- gets vmLabels
            case Map.lookup funcName labels of
              Just pc ->
                pushStack val >> jumpTo pc
              Nothing ->
                throwError $
                  RuntimeError $
                    "Label " ++ T.unpack funcName ++ " not found"
          VLeft err -> pushStack (VLeft err)
          _ -> throwError $ TypeError "EITHER_BIND requires Either value"

      -- Float operations
      PUSH_FLOAT f -> pushStack (VFloat f)
      -- Array operations
      PUSH_ARRAY n -> do
        elements <- popStackN n
        pushStack (VArray $ reverse elements)
      INDEX -> do
        idx <- popStack >>= toInt
        arr <- popStack
        case arr of
          VArray elements ->
            let i = fromInteger idx
             in if i >= 0 && i < length elements
                  then pushStack (elements !! i)
                  else
                    throwError $
                      RuntimeError $
                        "Array index out of bounds: "
                          ++ show idx
                          ++ " (array length: "
                          ++ show (length elements)
                          ++ ")"
          VTuple elements ->
            let i = fromInteger idx
             in if i >= 0 && i < length elements
                  then pushStack (elements !! i)
                  else
                    throwError $
                      RuntimeError $
                        "Tuple index out of bounds: "
                          ++ show idx
                          ++ " (tuple size: "
                          ++ show (length elements)
                          ++ ")"
          _ -> throwError $ TypeError $ "INDEX requires array or tuple, got " ++ show arr
      ARRAY_LENGTH -> do
        val <- popStack
        case val of
          VArray elements -> pushStack (VInt $ toInteger $ length elements)
          _ -> throwError $ TypeError $ "ARRAY_LENGTH requires array, got " ++ show val

      -- Cast operations (for now, just verify type compatibility and pass through)
      STATIC_CAST targetType -> do
        val <- popStack
        -- For now, do basic type checking
        case (val, T.unpack targetType) of
          (VInt _, t) | t `elem` ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"] -> pushStack val
          (VFloat _, t) | t `elem` ["f32", "f64"] -> pushStack val
          (VString _, "string") -> pushStack val
          (VBool _, "bool") -> pushStack val
          (VAtom _, "atom") -> pushStack val
          (VPid _, "pid") -> pushStack val
          (VNone, _) -> pushStack val -- None can be cast to any type
          _ -> throwError $ RuntimeError $ "STATIC_CAST: Cannot cast " ++ show val ++ " to " ++ T.unpack targetType
      REINTERPRET_CAST _targetType -> do
        -- Unsafe cast - just pass through the value without checking
        -- In a real implementation, this would do bitwise reinterpretation
        val <- popStack
        pushStack val
      CONST_CAST ->
        popStack >>= pushStack
      -- Function operations
      DEFINE_FUNCTION name params body ->
        let fdef =
              FunctionDef
                { funcDefName = name,
                  funcDefParams = params,
                  funcDefBody = body
                }
         in defineFunction fdef
      CALL_FUNCTION name _argCount -> do
        -- Get function definition
        fdef <- getFunctionDef name

        -- Arguments are already on stack in reverse order (last arg on top)
        -- The function body will bind them with STORE_LOCAL instructions

        -- Save current PC and locals
        savedPc <- getPc
        savedLocals <- gets vmLocals

        -- Clear locals for function scope
        modify $ \s -> s {vmLocals = Map.empty}

        -- Save current bytecode
        savedBytecode <- gets vmBytecode

        -- Execute function body (args are on stack)
        modify $ \s -> s {vmBytecode = funcDefBody fdef, vmPc = 0}
        executeFunctionBody (funcDefBody fdef)

        -- Restore bytecode, PC, and locals (result is on stack)
        modify $ \s -> s {vmBytecode = savedBytecode, vmPc = savedPc, vmLocals = savedLocals}

-- | Execute function body until RETURN
executeFunctionBody :: Bytecode -> VM ()
executeFunctionBody body = do
  pc <- getPc
  if pc >= length body
    then throwError $ RuntimeError "Function body ended without RETURN"
    else
      let instr = body !! pc
       in case instr of
            RETURN -> return ()
            _ ->
              executeInstruction instr
                >> incrementPc
                >> executeFunctionBody body

-- | Helper for binary operations (supports both Int and Float)
binaryOp :: (Integer -> Integer -> Integer) -> String -> VM ()
binaryOp op _name = do
  b <- popStack
  a <- popStack
  case (a, b) of
    (VInt x, VInt y) -> pushStack (VInt (op x y))
    (VFloat x, VFloat y) ->
      -- Convert operation to work on Doubles
      let opFloat = case _name of
            "ADD" -> (+)
            "SUB" -> (-)
            "MUL" -> (*)
            _ -> error "Unsupported float operation"
       in pushStack (VFloat (opFloat x y))
    (VInt x, VFloat y) ->
      let opFloat = case _name of
            "ADD" -> (+)
            "SUB" -> (-)
            "MUL" -> (*)
            _ -> error "Unsupported float operation"
       in pushStack (VFloat (opFloat (fromInteger x) y))
    (VFloat x, VInt y) ->
      let opFloat = case _name of
            "ADD" -> (+)
            "SUB" -> (-)
            "MUL" -> (*)
            _ -> error "Unsupported float operation"
       in pushStack (VFloat (opFloat x (fromInteger y)))
    _ -> throwError $ TypeError $ "Binary operation requires numeric values, got " ++ show a ++ " and " ++ show b

-- | Helper for comparison operations
comparisonOp :: (Value -> Value -> Bool) -> String -> VM ()
comparisonOp cmp _name = do
  b <- popStack
  a <- popStack
  pushStack (VBool (cmp a b))

-- | Helper for integer comparison operations (now supports floats too)
intComparisonOp :: (Integer -> Integer -> Bool) -> String -> VM ()
intComparisonOp cmp _name = do
  b <- popStack
  a <- popStack
  case (a, b) of
    (VInt x, VInt y) -> pushStack (VBool (cmp x y))
    (VFloat x, VFloat y) ->
      let cmpFloat = case _name of
            "CMP_LT" -> (<)
            "CMP_GT" -> (>)
            "CMP_LTE" -> (<=)
            "CMP_GTE" -> (>=)
            _ -> error "Unknown comparison"
       in pushStack (VBool (cmpFloat x y))
    (VInt x, VFloat y) ->
      let cmpFloat = case _name of
            "CMP_LT" -> (<)
            "CMP_GT" -> (>)
            "CMP_LTE" -> (<=)
            "CMP_GTE" -> (>=)
            _ -> error "Unknown comparison"
       in pushStack (VBool (cmpFloat (fromInteger x) y))
    (VFloat x, VInt y) ->
      let cmpFloat = case _name of
            "CMP_LT" -> (<)
            "CMP_GT" -> (>)
            "CMP_LTE" -> (<=)
            "CMP_GTE" -> (>=)
            _ -> error "Unknown comparison"
       in pushStack (VBool (cmpFloat x (fromInteger y)))
    _ -> throwError $ TypeError $ "Comparison requires numeric values, got " ++ show a ++ " and " ++ show b

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
    intercalate sep (x : xs) = x ++ sep ++ intercalate sep xs

-- | Execute bytecode program
executeBytecode :: Bytecode -> VM Value
executeBytecode code = do
  registerLabels code 0
  modify $ \s -> s {vmBytecode = code, vmPc = 0}
  executeLoop
  stack <- gets vmStack
  case stack of
    (x : _) -> return x
    [] -> return VUnit

-- | Register all labels in the bytecode
registerLabels :: Bytecode -> Int -> VM ()
registerLabels [] _ = return ()
registerLabels (LABEL name : rest) pc =
  registerLabel name pc >> registerLabels rest (pc + 1)
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
      when atBreakpoint $
        debugPutStrLn ("Breakpoint hit at PC=" ++ show pc)
      let instr = bytecode !! pc
      debugPutStrLn ("[EXEC] PC=" ++ show pc ++ " Instruction=" ++ show instr)
      case instr of
        HALT -> return ()
        RETURN -> return ()
        EXIT_PROCESS -> return ()
        _ ->
          executeInstruction instr
            >> incrementPc
            >> executeLoop

-- REPL functionality (currently unused but kept for future interactive mode)
_startREPL :: VM ()
_startREPL = do
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
          _startREPL
        c | c == T.pack ":state" -> do
          s <- getProcessState
          liftIO $ putStrLn $ "Process state: " ++ valueToString s
          _startREPL
        c | c == T.empty -> _startREPL
        _ ->
          pushStack (VString (T.pack line))
            >> liftIO (putStrLn "(pushed string onto stack)")
            >> _startREPL

-- | Execute process bytecode
executeProcessBytecode :: Bytecode -> VM Value
executeProcessBytecode code =
  modify (\s -> s {vmBytecode = code, vmPc = 0})
    >> registerLabels code 0
    >> executeLoop
    >> return VUnit

-- | Execute a receiving process (synchronous for now)
-- Process one message at a time, then return to allow more messages to be sent
executeReceivingProcess :: Pid -> VM ()
executeReceivingProcess pid = do
  debugPutStrLn $ "Executing process " ++ show pid
  processesVar <- gets vmProcesses
  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup pid processes
  case maybeProcess of
    Nothing -> debugPutStrLn $ "Process " ++ show pid ++ " not found"
    Just process -> do
      -- Save current Main state (PC, stack, locals, bytecode, currentPid)
      mainPc <- getPc
      mainStack <- gets vmStack
      mainLocals <- gets vmLocals
      mainBytecode <- gets vmBytecode
      mainCurrentPid <- gets vmCurrentPid
      mainLabels <- gets vmLabels

      debugPutStrLn $ "Switching to process " ++ show pid ++ ", saving Main PC=" ++ show mainPc

      -- Set up process state
      modify $ \s ->
        s
          { vmStack = processStack process,
            vmLocals = processLocals process,
            vmBytecode = processBytecode process,
            vmPc = processPc process,
            vmCurrentPid = Just pid,
            vmLabels = Map.empty -- Clear labels for fresh registration
          }

      -- Execute one iteration (process one message)
      bytecode <- gets vmBytecode
      debugPutStrLn $ "Registering labels for process " ++ show pid
      registerLabels bytecode 0
      debugPutStrLn $ "Starting execution loop for process " ++ show pid

      -- Execute until EXIT_PROCESS or error
      result <- catchError (executeLoop >> return (Right ())) (return . Left)

      -- Save process state back to process map
      processState' <- get

      -- Get the current process state value (which may have been updated by SET_STATE)
      currentStateValue <- catchError getProcessState (\_ -> return VUnit)

      -- Reload process from TVar to get any state changes
      maybeUpdatedProcess <- liftIO $ atomically $ do
        processes <- readTVar processesVar
        return $ Map.lookup pid processes

      case maybeUpdatedProcess of
        Nothing -> debugPutStrLn $ "Warning: Process " ++ show pid ++ " disappeared during execution"
        Just currentProcess -> do
          -- Find the receive_loop label PC
          labels <- gets vmLabels
          let loopPc = fromMaybe 0 (Map.lookup (T.pack "receive_loop") labels)
          debugPutStrLn $ "Process " ++ show pid ++ " message handled, resetting PC to receive_loop at PC=" ++ show loopPc ++ ", state=" ++ show currentStateValue

          -- Update the process in the map (clear stack, reset PC to receive_loop, preserve state)
          let updatedProcess =
                currentProcess
                  { processStack = [], -- Clear stack between messages
                    processLocals = vmLocals processState',
                    processPc = loopPc, -- Reset to receive loop start
                    processState = currentStateValue -- Preserve state!
                  }
          liftIO $ atomically $ modifyTVar processesVar (Map.insert pid updatedProcess)

      case result of
        Left err -> debugPutStrLn $ "Process " ++ show pid ++ " error: " ++ show err
        Right () -> debugPutStrLn $ "Process " ++ show pid ++ " finished processing message"

      -- Restore Main process context (but keep shared resources like processesVar and globals)
      debugPutStrLn $ "Restoring Main context with PC=" ++ show mainPc
      updatedProcesses <- gets vmProcesses
      updatedGlobals <- gets vmGlobals

      modify $ \s ->
        s
          { vmPc = mainPc, -- Restore exact PC where Main left off
            vmStack = mainStack,
            vmLocals = mainLocals,
            vmBytecode = mainBytecode,
            vmCurrentPid = mainCurrentPid,
            vmLabels = mainLabels,
            vmProcesses = updatedProcesses, -- Keep updated processes
            vmGlobals = updatedGlobals -- Keep updated globals
          }
