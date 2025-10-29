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
import System.IO (hFlush, stdout, isEOF)

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

-- | Convert a Value to a printable String
valueToString :: Value -> String
valueToString val = case val of
  VInt n -> show n
  VString s -> T.unpack s
  VAtom a -> ":" ++ T.unpack a
  VBool b -> if b then "true" else "false"
  VUnit -> "()"
  VNone -> "none"
  VPid n -> "<pid:" ++ show n ++ ">"
  VTuple elements -> "(" ++ intercalate ", " (map valueToString elements) ++ ")"
  where
    intercalate sep [] = ""
    intercalate sep [x] = x
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
  when (pc >= length bytecode) $
    return ()
  atBreakpoint <- checkBreakpoint
  when atBreakpoint $ do
    liftIO $ putStrLn $ "Breakpoint hit at PC=" ++ show pc
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
