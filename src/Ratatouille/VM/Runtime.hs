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
  , sendMessage
  , waitMessage
  , waitMessageWithSender
  , getProcessState
  , setProcessState
  , exitCurrentProcess
  , processMessageLoop
  , getAllProcesses
  , killProcess
  )
where

import Ratatouille.Bytecode.Types
import Ratatouille.VM.VM
import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent (killThread)
import Control.Concurrent.STM (atomically, newTQueue, readTVar, writeTVar, writeTQueue, tryReadTQueue, modifyTVar)
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
createProcessInstance :: Text -> [Value] -> VM Pid
createProcessInstance name args = do
  pdef <- getProcessDef name
  pid <- allocatePid
  mailbox <- liftIO $ atomically newTQueue
  let process = Process
        { processId = pid
        , processStack = args  -- Initialize stack with arguments
        , processLocals = Map.empty
        , processState = VNone
        , processMailbox = mailbox
        , processPc = 0
        , processBytecode = procBody pdef
        , processThreadId = Nothing
        }
  
  -- For now, don't fork a thread - we'll run processes synchronously
  -- But still add the process to the process map for message sending
  processesVar <- gets vmProcesses
  liftIO $ atomically $ modifyTVar processesVar (Map.insert pid process)
  return pid

-- | Run a process in its own thread
runProcessThread :: Pid -> VMState -> IO ()  
runProcessThread pid initialState = do
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
        -- Execute the bytecode using the standard execution loop
        modify $ \s -> s { vmBytecode = processBytecode process, vmPc = 0 }
        return VUnit
      case result of
        Left err -> putStrLn $ "Process " ++ show pid ++ " error: " ++ show err
        Right _ -> putStrLn $ "Process " ++ show pid ++ " completed"
      atomically $ modifyTVar processesVar (Map.delete pid)


-- | Send a message to a process
sendMessage :: Pid -> Value -> VM ()
sendMessage targetPid msg = do
  senderPid <- getCurrentPid
  processesVar <- gets vmProcesses
  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup targetPid processes
  case maybeProcess of
    Nothing -> throwError $ ProcessError $ T.pack $ "Process not found: " ++ show targetPid
    Just process -> do
      let message = Message { msgSender = senderPid, msgContent = msg }
      debugPutStrLn $ "Sending message to mailbox of process " ++ show targetPid
      liftIO $ atomically $ writeTQueue (processMailbox process) message
      debugPutStrLn $ "Message written to mailbox"

-- | Wait for the next message in the current process's mailbox
waitMessage :: VM Value
waitMessage = do
  pid <- getCurrentPid
  debugPutStrLn $ "waitMessage: Current PID is " ++ show pid
  processesVar <- gets vmProcesses
  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup pid processes
  case maybeProcess of
    Nothing -> throwError $ ProcessError $ T.pack $ "Current process not found: " ++ show pid
    Just process -> do
      debugPutStrLn $ "waitMessage: Found process " ++ show pid ++ " in map"
      maybeMsg <- liftIO $ atomically $ tryReadTQueue (processMailbox process)
      case maybeMsg of
        Nothing -> do
          debugPutStrLn $ "waitMessage: No message in mailbox for process " ++ show pid
          throwError $ ProcessError $ T.pack $ "No message in mailbox for process " ++ show pid
        Just msg -> do
          debugPutStrLn $ "waitMessage: Found message " ++ show (msgContent msg)
          return (msgContent msg)

-- | Wait for a message and return both the message and the sender PID
waitMessageWithSender :: VM (Value, Pid)
waitMessageWithSender = do
  pid <- getCurrentPid
  debugPutStrLn $ "waitMessageWithSender: Current PID is " ++ show pid
  processesVar <- gets vmProcesses
  maybeProcess <- liftIO $ atomically $ do
    processes <- readTVar processesVar
    return $ Map.lookup pid processes
  case maybeProcess of
    Nothing -> throwError $ ProcessError $ T.pack $ "Current process not found: " ++ show pid
    Just process -> do
      debugPutStrLn $ "waitMessageWithSender: Found process " ++ show pid ++ " in map"
      maybeMsg <- liftIO $ atomically $ tryReadTQueue (processMailbox process)
      case maybeMsg of
        Nothing -> do
          debugPutStrLn $ "waitMessageWithSender: No message in mailbox for process " ++ show pid
          throwError $ ProcessError $ T.pack $ "No message in mailbox for process " ++ show pid
        Just msg -> do
          debugPutStrLn $ "waitMessageWithSender: Found message " ++ show (msgContent msg) ++ " from " ++ show (msgSender msg)
          return (msgContent msg, msgSender msg)

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
  liftIO $ atomically $ modifyTVar processesVar (Map.delete pid)
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
  msg <- waitMessage
  pushStack msg
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
      case processThreadId process of
        Just tid -> liftIO $ killThread tid
        Nothing -> return ()
      liftIO $ atomically $ modifyTVar processesVar (Map.delete pid)
