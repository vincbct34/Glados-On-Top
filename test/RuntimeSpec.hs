{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Runtime module tests
-}

module RuntimeSpec (spec) where

import Test.Hspec
import Ratatouille.VM.VM
import Ratatouille.VM.Runtime
import Ratatouille.VM.Interpreter (executeProcessBytecode)
import Ratatouille.Bytecode.Types
import Control.Concurrent.STM
import qualified Data.Map as Map
import qualified Data.Text as T

-- Helper to create a basic VM state for testing
createTestVMState :: IO VMState
createTestVMState = do
  processesVar <- newTVarIO Map.empty
  pidVar <- newTVarIO (Pid 1)
  return $ VMState
    { vmStack = []
    , vmGlobals = Map.empty
    , vmLocals = Map.empty
    , vmPc = 0
    , vmBytecode = []
    , vmLabels = Map.empty
    , vmProcessDefs = Map.empty
    , vmProcesses = processesVar
    , vmNextPid = pidVar
    , vmCurrentPid = Nothing
    , vmDebugMode = False
    , vmBreakpoints = []
    , vmTraceEnabled = False
    }

-- Helper to run VM with a current process
runVMWithProcess :: Pid -> VM a -> IO (Either VMError a, VMState)
runVMWithProcess pid action = do
  state <- createTestVMState
  mailbox <- newTQueueIO
  let process = Process
        { processId = pid
        , processStack = []
        , processLocals = Map.empty
        , processState = VNone
        , processMailbox = mailbox
        , processPc = 0
        , processBytecode = []
        , processThreadId = Nothing
        }
  atomically $ modifyTVar (vmProcesses state) (Map.insert pid process)
  let stateWithPid = state { vmCurrentPid = Just pid }
  executeVM stateWithPid action

spec :: Spec
spec = do
  describe "getCurrentPid" $ do
    it "returns current process PID" $ do
      (result, _) <- runVMWithProcess (Pid 5) getCurrentPid
      result `shouldBe` Right (Pid 5)

    it "returns error when no current process" $ do
      state <- createTestVMState
      (result, _) <- executeVM state getCurrentPid
      result `shouldBe` Left (ProcessError (T.pack "No current process"))

  describe "fromPid" $ do
    it "converts Pid to Integer" $ do
      fromPid (Pid 42) `shouldBe` 42
      fromPid (Pid 0) `shouldBe` 0
      fromPid (Pid 999) `shouldBe` 999

  describe "allocatePid" $ do
    it "allocates sequential PIDs" $ do
      state <- createTestVMState
      (result1, state1) <- executeVM state allocatePid
      (result2, _) <- executeVM state1 allocatePid
      result1 `shouldBe` Right (Pid 1)
      result2 `shouldBe` Right (Pid 2)

    it "allocates unique PIDs" $ do
      state <- createTestVMState
      (result1, state1) <- executeVM state allocatePid
      (result2, state2) <- executeVM state1 allocatePid
      (result3, _) <- executeVM state2 allocatePid
      result1 `shouldNotBe` result2
      result2 `shouldNotBe` result3
      result1 `shouldNotBe` result3

  describe "createProcessInstance" $ do
    it "creates a new process instance" $ do
      state <- createTestVMState
      let pdef = ProcessDef (T.pack "TestProc") [] [PUSH_INT 0, HALT]
      let stateWithDef = state { vmProcessDefs = Map.singleton (T.pack "TestProc") pdef }
      (result, finalState) <- executeVM stateWithDef $ createProcessInstance (T.pack "TestProc") []
      case result of
        Right pid -> do
          pid `shouldSatisfy` (\(Pid n) -> n >= 1)
          processes <- atomically $ readTVar (vmProcesses finalState)
          Map.member pid processes `shouldBe` True
        Left err -> expectationFailure $ "Expected success, got error: " ++ show err

    it "returns error for undefined process" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ createProcessInstance (T.pack "MissingProc") []
      result `shouldBe` Left (UndefinedProcess (T.pack "MissingProc"))

  describe "sendMessage" $ do
    it "sends message to another process" $ do
      state <- createTestVMState
      -- Create sender process
      mailbox1 <- newTQueueIO
      let sender = Process (Pid 1) [] Map.empty VNone mailbox1 0 [] Nothing
      -- Create receiver process
      mailbox2 <- newTQueueIO
      let receiver = Process (Pid 2) [] Map.empty VNone mailbox2 0 [] Nothing
      atomically $ do
        modifyTVar (vmProcesses state) (Map.insert (Pid 1) sender)
        modifyTVar (vmProcesses state) (Map.insert (Pid 2) receiver)

      let stateWithPid = state { vmCurrentPid = Just (Pid 1) }
      (result, _) <- executeVM stateWithPid $ sendMessage (Pid 2) (VInt 42)

      result `shouldBe` Right ()
      -- Check message was sent
      msg <- atomically $ tryReadTQueue mailbox2
      case msg of
        Just message -> do
          msgSender message `shouldBe` Pid 1
          msgContent message `shouldBe` VInt 42
        Nothing -> expectationFailure "No message in mailbox"

    it "returns error when sending to non-existent process" $ do
      (result, _) <- runVMWithProcess (Pid 1) $ sendMessage (Pid 999) (VInt 42)
      result `shouldBe` Left (ProcessError (T.pack "Process not found: Pid 999"))

  describe "waitMessage" $ do
    it "receives a message from mailbox" $ do
      state <- createTestVMState
      mailbox <- newTQueueIO
      let message = Message (Pid 2) (VInt 42)
      atomically $ writeTQueue mailbox message

      let process = Process (Pid 1) [] Map.empty VNone mailbox 0 [] Nothing
      atomically $ modifyTVar (vmProcesses state) (Map.insert (Pid 1) process)

      let stateWithPid = state { vmCurrentPid = Just (Pid 1) }
      (result, _) <- executeVM stateWithPid waitMessage
      result `shouldBe` Right (VInt 42)

    it "returns error when process not found" $ do
      state <- createTestVMState
      let stateWithPid = state { vmCurrentPid = Just (Pid 999) }
      (result, _) <- executeVM stateWithPid waitMessage
      result `shouldBe` Left (ProcessError (T.pack "Current process not found: Pid 999"))

  describe "getProcessState and setProcessState" $ do
    it "gets initial process state" $ do
      (result, _) <- runVMWithProcess (Pid 1) getProcessState
      result `shouldBe` Right VNone

    it "sets and gets process state" $ do
      (result, _) <- runVMWithProcess (Pid 1) $ do
        setProcessState (VInt 100)
        getProcessState
      result `shouldBe` Right (VInt 100)

    it "updates process state" $ do
      (result, _) <- runVMWithProcess (Pid 1) $ do
        setProcessState (VInt 1)
        setProcessState (VInt 2)
        setProcessState (VInt 3)
        getProcessState
      result `shouldBe` Right (VInt 3)

    it "returns error when process not found" $ do
      state <- createTestVMState
      let stateWithPid = state { vmCurrentPid = Just (Pid 999) }
      (result, _) <- executeVM stateWithPid getProcessState
      result `shouldBe` Left (ProcessError (T.pack "Current process not found: Pid 999"))

  describe "exitCurrentProcess" $ do
    it "removes process from registry" $ do
      state <- createTestVMState
      mailbox <- newTQueueIO
      let process = Process (Pid 1) [] Map.empty VNone mailbox 0 [] Nothing
      atomically $ modifyTVar (vmProcesses state) (Map.insert (Pid 1) process)

      let stateWithPid = state { vmCurrentPid = Just (Pid 1) }
      (result, finalState) <- executeVM stateWithPid exitCurrentProcess
      result `shouldBe` Right ()

      processes <- atomically $ readTVar (vmProcesses finalState)
      Map.member (Pid 1) processes `shouldBe` False

  describe "getAllProcesses" $ do
    it "returns empty list when no processes" $ do
      state <- createTestVMState
      (result, _) <- executeVM state getAllProcesses
      result `shouldBe` Right []

    it "returns all processes" $ do
      state <- createTestVMState
      mailbox1 <- newTQueueIO
      mailbox2 <- newTQueueIO
      let process1 = Process (Pid 1) [] Map.empty VNone mailbox1 0 [] Nothing
      let process2 = Process (Pid 2) [] Map.empty VNone mailbox2 0 [] Nothing
      atomically $ do
        modifyTVar (vmProcesses state) (Map.insert (Pid 1) process1)
        modifyTVar (vmProcesses state) (Map.insert (Pid 2) process2)

      (result, _) <- executeVM state getAllProcesses
      case result of
        Right procs -> length procs `shouldBe` 2
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

  describe "killProcess" $ do
    it "kills a specific process" $ do
      state <- createTestVMState
      mailbox <- newTQueueIO
      let process = Process (Pid 5) [] Map.empty VNone mailbox 0 [] Nothing
      atomically $ modifyTVar (vmProcesses state) (Map.insert (Pid 5) process)

      (result, finalState) <- executeVM state $ killProcess (Pid 5)
      result `shouldBe` Right ()

      processes <- atomically $ readTVar (vmProcesses finalState)
      Map.member (Pid 5) processes `shouldBe` False

    it "returns error when killing non-existent process" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ killProcess (Pid 999)
      result `shouldBe` Left (ProcessError (T.pack "Process not found: Pid 999"))

  describe "processMessageLoop" $ do
    it "waits for message and pushes to stack" $ do
      state <- createTestVMState
      mailbox <- newTQueueIO
      let message = Message (Pid 2) (VString (T.pack "hello"))
      atomically $ writeTQueue mailbox message

      let process = Process (Pid 1) [] Map.empty VNone mailbox 0 [] Nothing
      atomically $ modifyTVar (vmProcesses state) (Map.insert (Pid 1) process)

      let stateWithPid = state { vmCurrentPid = Just (Pid 1) }
      (result, finalState) <- executeVM stateWithPid processMessageLoop
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VString (T.pack "hello")]

  describe "executeProcessBytecode" $ do
    it "sets bytecode and PC correctly" $ do
      state <- createTestVMState
      mailbox <- newTQueueIO
      -- Pre-fill mailbox to prevent blocking
      atomically $ writeTQueue mailbox (Message (Pid 2) VUnit)
      let process = Process (Pid 1) [] Map.empty VNone mailbox 0 [] Nothing
      atomically $ modifyTVar (vmProcesses state) (Map.insert (Pid 1) process)
      let stateWithPid = state { vmCurrentPid = Just (Pid 1) }
      (result, finalState) <- executeVM stateWithPid $ do
        _ <- executeProcessBytecode [PUSH_INT 1, PUSH_INT 2]
        return ()
      result `shouldBe` Right ()
      vmBytecode finalState `shouldBe` [PUSH_INT 1, PUSH_INT 2]
      vmPc finalState `shouldBe` 2  -- PC advances after executing 2 instructions

  describe "Process instance creation with thread" $ do
    it "creates process with thread ID" $ do
      state <- createTestVMState
      let pdef = ProcessDef (T.pack "SimpleProc") [] [HALT]
      let stateWithDef = state { vmProcessDefs = Map.singleton (T.pack "SimpleProc") pdef }
      (result, finalState) <- executeVM stateWithDef $ createProcessInstance (T.pack "SimpleProc") []
      case result of
        Right pid -> do
          processes <- atomically $ readTVar (vmProcesses finalState)
          case Map.lookup pid processes of
            Just proc -> processId proc `shouldBe` pid
            Nothing -> expectationFailure "Process not found in registry"
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

  describe "Multiple process operations" $ do
    it "handles multiple processes simultaneously" $ do
      state <- createTestVMState
      mailbox1 <- newTQueueIO
      mailbox2 <- newTQueueIO
      mailbox3 <- newTQueueIO
      let process1 = Process (Pid 1) [VInt 1] Map.empty VNone mailbox1 0 [] Nothing
      let process2 = Process (Pid 2) [VInt 2] Map.empty VNone mailbox2 0 [] Nothing
      let process3 = Process (Pid 3) [VInt 3] Map.empty VNone mailbox3 0 [] Nothing
      atomically $ do
        modifyTVar (vmProcesses state) (Map.insert (Pid 1) process1)
        modifyTVar (vmProcesses state) (Map.insert (Pid 2) process2)
        modifyTVar (vmProcesses state) (Map.insert (Pid 3) process3)

      (result, _) <- executeVM state getAllProcesses
      case result of
        Right procs -> length procs `shouldBe` 3
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

  describe "Message passing between processes" $ do
    it "sends multiple messages to same process" $ do
      state <- createTestVMState
      mailbox1 <- newTQueueIO
      mailbox2 <- newTQueueIO
      let sender = Process (Pid 1) [] Map.empty VNone mailbox1 0 [] Nothing
      let receiver = Process (Pid 2) [] Map.empty VNone mailbox2 0 [] Nothing
      atomically $ do
        modifyTVar (vmProcesses state) (Map.insert (Pid 1) sender)
        modifyTVar (vmProcesses state) (Map.insert (Pid 2) receiver)

      let stateWithPid = state { vmCurrentPid = Just (Pid 1) }
      (result, _) <- executeVM stateWithPid $ do
        sendMessage (Pid 2) (VInt 1)
        sendMessage (Pid 2) (VInt 2)
        sendMessage (Pid 2) (VInt 3)
      result `shouldBe` Right ()

      -- Verify messages are in queue
      msg1 <- atomically $ tryReadTQueue mailbox2
      msg2 <- atomically $ tryReadTQueue mailbox2
      msg3 <- atomically $ tryReadTQueue mailbox2
      msg1 `shouldSatisfy` isJust
      msg2 `shouldSatisfy` isJust
      msg3 `shouldSatisfy` isJust

  describe "Process state transitions" $ do
    it "transitions through multiple states" $ do
      (result, _) <- runVMWithProcess (Pid 1) $ do
        setProcessState (VInt 0)
        setProcessState (VAtom (T.pack "running"))
        setProcessState (VTuple [VInt 1, VInt 2])
        getProcessState
      result `shouldBe` Right (VTuple [VInt 1, VInt 2])

    it "maintains independent state per process" $ do
      state <- createTestVMState
      mailbox1 <- newTQueueIO
      mailbox2 <- newTQueueIO
      let process1 = Process (Pid 1) [] Map.empty (VInt 100) mailbox1 0 [] Nothing
      let process2 = Process (Pid 2) [] Map.empty (VInt 200) mailbox2 0 [] Nothing
      atomically $ do
        modifyTVar (vmProcesses state) (Map.insert (Pid 1) process1)
        modifyTVar (vmProcesses state) (Map.insert (Pid 2) process2)

      let state1 = state { vmCurrentPid = Just (Pid 1) }
      let state2 = state { vmCurrentPid = Just (Pid 2) }
      (result1, _) <- executeVM state1 getProcessState
      (result2, _) <- executeVM state2 getProcessState
      result1 `shouldBe` Right (VInt 100)
      result2 `shouldBe` Right (VInt 200)

  describe "runProcessThread" $ do
    it "handles process not found" $ do
      state <- createTestVMState
      -- Don't insert a process, so lookup will fail
      runProcessThread (Pid 999) state
      -- This should print error message but not crash
      True `shouldBe` True

  describe "Edge cases for process operations" $ do
    it "handles getCurrentPid with various PIDs" $ do
      (result1, _) <- runVMWithProcess (Pid 0) getCurrentPid
      result1 `shouldBe` Right (Pid 0)
      (result2, _) <- runVMWithProcess (Pid 999999) getCurrentPid
      result2 `shouldBe` Right (Pid 999999)

    it "handles fromPid with large numbers" $ do
      fromPid (Pid 1000000) `shouldBe` 1000000
      fromPid (Pid (-1)) `shouldBe` (-1)

  describe "Message operations" $ do
    it "handles Message Show instance" $ do
      let msg = Message (Pid 1) (VInt 42)
      show msg `shouldContain` "Message"
      show msg `shouldContain` "Pid 1"

    it "handles Message equality" $ do
      let msg1 = Message (Pid 1) (VString (T.pack "test"))
      let msg2 = Message (Pid 1) (VString (T.pack "test"))
      let msg3 = Message (Pid 2) (VString (T.pack "test"))
      msg1 `shouldBe` msg2
      msg1 `shouldNotBe` msg3

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
