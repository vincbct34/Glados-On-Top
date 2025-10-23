{-
-- EPITECH PROJECT, 2025
-- glados-vm
-- File description:
-- Runtime and process management
-}

module Ratatouille.VM.Runtime 
  ( getCurrentPid
  , fromPid
  , allocatePid
  , createProcessInstance
  , runProcessThread
  , executeProcessBytecode
  , sendMessage
  , waitMessage
  , getProcessState
  , setProcessState
  , exitCurrentProcess
  , processMessageLoop
  , getAllProcesses
  , killProcess
  ) where

import Ratatouille.Bytecode.Types
import Ratatouille.VM.VM
import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Get the current process PID
getCurrentPid :: VM Pid
getCurrentPid = do
  maybePid <- gets vmCurrentPid
  case maybePid of
    Nothing -> throwError $ ProcessError (T.pack "No current process")
    Just pid -> return pid

-- | Convert Pid to Integer
fromPid :: Pid -> Integer
fromPid (Pid n) = n

-- | Allocate a new process ID
allocatePid :: VM Pid
allocatePid = do
  pidVar <- gets vmNextPid
  liftIO $ atomically $ do
    pid <- readTVar pidVar
    writeTVar pidVar (pid + 1)
    return pid

-- | Create a new process instance from a process definition
createProcessInstance :: Text -> VM Pid
createProcessInstance name = do
  -- Get process definition
  pdef <- getProcessDef name

  -- Allocate new PID
  pid <- allocatePid

  -- Create mailbox
  mailbox <- liftIO $ atomically newTQueue

  -- Create process instance
  let process = Process
        { processId = pid
        , processStack = []
        , processLocals = Map.empty
        , processState = VNone
        , processMailbox = mailbox
        , processPc = 0
        , processBytecode = procBody pdef
        , processThreadId = Nothing
        }

  -- Register process
  processesVar <- gets vmProcesses
  liftIO $ atomically $ modifyTVar processesVar (Map.insert pid process)

  -- Fork a thread to run the process
  vmState <- get
  threadId <- liftIO $ forkIO $ runProcessThread pid vmState

  -- Update process with thread ID
  liftIO $ atomically $ modifyTVar processesVar $
    Map.adjust (\p -> p { processThreadId = Just threadId }) pid

  return pid

-- | Run a process in its own thread
runProcessThread :: Pid -> VMState -> IO ()
runProcessThread pid initialState = do
  -- Get the process
  processesVar <- return $ vmProcesses initialState
  maybeProcess <- atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup pid processes

  case maybeProcess of
    Nothing -> putStrLn $ "Error: Process " ++ show pid ++ " not found"
    Just process -> do
      -- Create process-local VM state
      let procVMState = initialState
            { vmStack = processStack process
            , vmLocals = processLocals process
            , vmBytecode = processBytecode process
            , vmPc = processPc process
            , vmCurrentPid = Just pid
            }

      -- Run the process bytecode
      (result, _finalState) <- executeVM procVMState $ do
        -- Import the Interpreter module function
        executeProcessBytecode (processBytecode process)

      case result of
        Left err -> putStrLn $ "Process " ++ show pid ++ " error: " ++ show err
        Right _ -> putStrLn $ "Process " ++ show pid ++ " completed"

      -- Remove process from registry
      atomically $ modifyTVar processesVar (Map.delete pid)

-- | Execute process bytecode (imported from Interpreter)
-- This is a placeholder that will be properly implemented
executeProcessBytecode :: Bytecode -> VM Value
executeProcessBytecode code = do
  modify $ \s -> s { vmBytecode = code, vmPc = 0 }
  -- Execute main process loop
  processMessageLoop
  return VUnit

-- | Send a message to a process
sendMessage :: Pid -> Value -> VM ()
sendMessage targetPid msg = do
  senderPid <- getCurrentPid
  processesVar <- gets vmProcesses

  -- Look up target process
  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup targetPid processes

  case maybeProcess of
    Nothing -> throwError $ ProcessError $ T.pack $ "Process not found: " ++ show targetPid
    Just process -> do
      let message = Message { msgSender = senderPid, msgContent = msg }
      liftIO $ atomically $ writeTQueue (processMailbox process) message

-- | Wait for the next message in the current process's mailbox
waitMessage :: VM Value
waitMessage = do
  pid <- getCurrentPid
  processesVar <- gets vmProcesses

  -- Get current process
  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup pid processes

  case maybeProcess of
    Nothing -> throwError $ ProcessError $ T.pack $ "Current process not found: " ++ show pid
    Just process -> do
      -- Wait for message (blocking)
      msg <- liftIO $ atomically $ readTQueue (processMailbox process)
      return (msgContent msg)

-- | Get process state
getProcessState :: VM Value
getProcessState = do
  pid <- getCurrentPid
  processesVar <- gets vmProcesses

  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup pid processes

  case maybeProcess of
    Nothing -> throwError $ ProcessError $ T.pack $ "Current process not found: " ++ show pid
    Just process -> return (processState process)

-- | Set process state
setProcessState :: Value -> VM ()
setProcessState newState = do
  pid <- getCurrentPid
  processesVar <- gets vmProcesses

  liftIO $ atomically $ modifyTVar processesVar $
    Map.adjust (\p -> p { processState = newState }) pid

-- | Exit the current process
exitCurrentProcess :: VM ()
exitCurrentProcess = do
  pid <- getCurrentPid
  processesVar <- gets vmProcesses

  -- Remove process from registry
  liftIO $ atomically $ modifyTVar processesVar (Map.delete pid)

  -- Kill the thread (if we have thread ID)
  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup pid processes

  case maybeProcess of
    Just process -> case processThreadId process of
      Just tid -> liftIO $ killThread tid
      Nothing -> return ()
    Nothing -> return ()

-- | Main process message loop
processMessageLoop :: VM ()
processMessageLoop = do
  -- Wait for a message
  msg <- waitMessage
  pushStack msg

  -- Continue execution (the bytecode should handle the message)
  -- In a full implementation, this would dispatch to message handlers
  return ()

-- | Get all running processes (for debugging)
getAllProcesses :: VM [(Pid, Process)]
getAllProcesses = do
  processesVar <- gets vmProcesses
  processes <- liftIO $ atomically $ readTVar processesVar
  return $ Map.toList processes

-- | Kill a specific process
killProcess :: Pid -> VM ()
killProcess pid = do
  processesVar <- gets vmProcesses

  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup pid processes

  case maybeProcess of
    Nothing -> throwError $ ProcessError $ T.pack $ "Process not found: " ++ show pid
    Just process -> do
      -- Kill thread if exists
      case processThreadId process of
        Just tid -> liftIO $ killThread tid
        Nothing -> return ()

      -- Remove from registry
      liftIO $ atomically $ modifyTVar processesVar (Map.delete pid)
