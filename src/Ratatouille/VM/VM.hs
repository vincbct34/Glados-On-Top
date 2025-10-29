{-
-- EPITECH PROJECT, 2025
-- glados-vm
-- File description:
-- VM core types and state management
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ratatouille.VM.VM 
  ( -- * VM Types
    VM(..)
  , VMState(..)
  , VMError(..)
  , Pid(..)
  , Message(..)
  , ProcessDef(..)
  , Process(..)
  -- * Execution
  , executeVM
  -- * Stack Operations
  , pushStack
  , popStack
  , peekStack
  , popStackN
  -- * Variable Operations
  , loadGlobal
  , storeGlobal
  , loadLocal
  , storeLocal
  -- * Control Flow
  , getPc
  , setPc
  , incrementPc
  , jump
  , jumpTo
  , registerLabel
  , findLabel
  -- * Process Management
  , defineProcess
  , getProcessDef
  -- * Debug & Trace
  , isDebugMode
  , isTraceEnabled
  , traceInstruction
  , checkBreakpoint
  -- * Helper Functions
  , toBool
  , toInt
  , toString
  , toTuple
  , toPid
  ) where

import Ratatouille.Bytecode.Types
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM (TQueue, TVar)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

-- | VM Errors
data VMError
  = StackUnderflow
  | TypeError String
  | UndefinedVariable Text
  | UndefinedProcess Text
  | InvalidJump Int
  | InvalidLabel Text
  | DivisionByZero
  | ProcessError Text
  | PatternMatchFailed
  | RuntimeError String
  deriving (Show, Eq)

-- | Process identifier
newtype Pid = Pid Integer
  deriving (Show, Eq, Ord, Num)

-- | Message in a process mailbox
data Message = Message
  { msgSender :: Pid
  , msgContent :: Value
  } deriving (Show, Eq)

-- | Process definition (template)
data ProcessDef = ProcessDef
  { procName :: Text
  , procParams :: [Text]
  , procBody :: Bytecode
  } deriving (Show, Eq)

-- | Runtime process instance
data Process = Process
  { processId :: Pid
  , processStack :: [Value]
  , processLocals :: Map Text Value
  , processState :: Value
  , processMailbox :: TQueue Message
  , processPc :: Int
  , processBytecode :: Bytecode
  , processThreadId :: Maybe ThreadId
  } deriving (Eq)

instance Show Process where
  show p = "Process{pid=" ++ show (processId p) ++
           ", stack=" ++ show (processStack p) ++
           ", locals=" ++ show (processLocals p) ++
           ", state=" ++ show (processState p) ++
           ", pc=" ++ show (processPc p) ++ "}"

-- | VM State
data VMState = VMState
  { vmStack :: [Value]
  , vmGlobals :: Map Text Value
  , vmLocals :: Map Text Value
  , vmPc :: Int
  , vmBytecode :: Bytecode
  , vmLabels :: Map Text Int
  , vmProcessDefs :: Map Text ProcessDef
  , vmProcesses :: TVar (Map Pid Process)
  , vmNextPid :: TVar Pid
  , vmCurrentPid :: Maybe Pid
  , vmDebugMode :: Bool
  , vmBreakpoints :: [Int]
  , vmTraceEnabled :: Bool
  }

-- | VM Monad combining State and Exception handling
newtype VM a = VM { runVM :: ExceptT VMError (StateT VMState IO) a }
  deriving (Functor, Applicative, Monad, MonadState VMState, MonadError VMError, MonadIO)

-- | Execute VM computation
executeVM :: VMState -> VM a -> IO (Either VMError a, VMState)
executeVM initialState (VM action) = runStateT (runExceptT action) initialState

-- | Stack operations
pushStack :: Value -> VM ()
pushStack v = modify $ \s -> s { vmStack = v : vmStack s }

popStack :: VM Value
popStack = do
  stack <- gets vmStack
  case stack of
    [] -> throwError StackUnderflow
    (x:xs) -> do
      modify $ \s -> s { vmStack = xs }
      return x

peekStack :: VM Value
peekStack = do
  stack <- gets vmStack
  case stack of
    [] -> throwError StackUnderflow
    (x:_) -> return x

popStackN :: Int -> VM [Value]
popStackN n = replicateM n popStack

-- | Variable operations
loadGlobal :: Text -> VM Value
loadGlobal name = do
  globals <- gets vmGlobals
  case Map.lookup name globals of
    Nothing -> throwError $ UndefinedVariable name
    Just v -> return v

storeGlobal :: Text -> Value -> VM ()
storeGlobal name val =
  modify $ \s -> s { vmGlobals = Map.insert name val (vmGlobals s) }

loadLocal :: Text -> VM Value
loadLocal name = do
  locals <- gets vmLocals
  case Map.lookup name locals of
    Nothing -> throwError $ UndefinedVariable name
    Just v -> return v

storeLocal :: Text -> Value -> VM ()
storeLocal name val =
  modify $ \s -> s { vmLocals = Map.insert name val (vmLocals s) }

-- | PC operations
getPc :: VM Int
getPc = gets vmPc

setPc :: Int -> VM ()
setPc pc = modify $ \s -> s { vmPc = pc }

incrementPc :: VM ()
incrementPc = modify $ \s -> s { vmPc = vmPc s + 1 }

jump :: Int -> VM ()
jump offset = do
  pc <- getPc
  let newPc = pc + offset
  bytecode <- gets vmBytecode
  when (newPc < 0 || newPc >= length bytecode) $
    throwError $ InvalidJump newPc
  setPc newPc

jumpTo :: Int -> VM ()
jumpTo pc = do
  bytecode <- gets vmBytecode
  when (pc < 0 || pc >= length bytecode) $
    throwError $ InvalidJump pc
  setPc pc

-- | Label operations
registerLabel :: Text -> Int -> VM ()
registerLabel name pc =
  modify $ \s -> s { vmLabels = Map.insert name pc (vmLabels s) }

findLabel :: Text -> VM Int
findLabel name = do
  labels <- gets vmLabels
  case Map.lookup name labels of
    Nothing -> throwError $ InvalidLabel name
    Just pc -> return pc

-- | Process definition operations
defineProcess :: ProcessDef -> VM ()
defineProcess pdef =
  modify $ \s -> s { vmProcessDefs = Map.insert (procName pdef) pdef (vmProcessDefs s) }

getProcessDef :: Text -> VM ProcessDef
getProcessDef name = do
  defs <- gets vmProcessDefs
  case Map.lookup name defs of
    Nothing -> throwError $ UndefinedProcess name
    Just pdef -> return pdef

-- | Debug operations
isDebugMode :: VM Bool
isDebugMode = gets vmDebugMode

isTraceEnabled :: VM Bool
isTraceEnabled = gets vmTraceEnabled

traceInstruction :: Instruction -> VM ()
traceInstruction instr = do
  enabled <- isTraceEnabled
  when enabled $ do
    pc <- getPc
    stack <- gets vmStack
    liftIO $ putStrLn $ "[TRACE] PC=" ++ show pc ++ " | " ++ show instr ++ " | Stack=" ++ show stack

checkBreakpoint :: VM Bool
checkBreakpoint = do
  pc <- getPc
  breakpoints <- gets vmBreakpoints
  return $ pc `elem` breakpoints

-- | Helper functions
toBool :: Value -> VM Bool
toBool (VBool b) = return b
toBool v = throwError $ TypeError $ "Expected Bool, got " ++ show v

toInt :: Value -> VM Integer
toInt (VInt i) = return i
toInt v = throwError $ TypeError $ "Expected Int, got " ++ show v

toString :: Value -> VM Text
toString (VString s) = return s
toString v = throwError $ TypeError $ "Expected String, got " ++ show v

toTuple :: Value -> VM [Value]
toTuple (VTuple xs) = return xs
toTuple v = throwError $ TypeError $ "Expected Tuple, got " ++ show v

toPid :: Value -> VM Pid
toPid (VPid n) = return (Pid n)
toPid v = throwError $ TypeError $ "Expected Pid, got " ++ show v
