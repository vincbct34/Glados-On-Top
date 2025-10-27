{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Interpreter module tests
-}

module InterpreterSpec (spec) where

import Test.Hspec
import Ratatouille.VM.VM
import Ratatouille.VM.Interpreter
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
  describe "Stack instructions" $ do
    it "executes PUSH_INT" $ do
      state <- createTestVMState
      (result, finalState) <- executeVM state $ executeInstruction (PUSH_INT 42)
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 42]

    it "executes PUSH_STRING" $ do
      state <- createTestVMState
      (result, finalState) <- executeVM state $ executeInstruction (PUSH_STRING (T.pack "hello"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VString (T.pack "hello")]

    it "executes PUSH_ATOM" $ do
      state <- createTestVMState
      (result, finalState) <- executeVM state $ executeInstruction (PUSH_ATOM (T.pack "ok"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VAtom (T.pack "ok")]

    it "executes PUSH_UNIT" $ do
      state <- createTestVMState
      (result, finalState) <- executeVM state $ executeInstruction PUSH_UNIT
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VUnit]

    it "executes PUSH_NONE" $ do
      state <- createTestVMState
      (result, finalState) <- executeVM state $ executeInstruction PUSH_NONE
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VNone]

    it "executes PUSH_BOOL" $ do
      state <- createTestVMState
      (result, finalState) <- executeVM state $ executeInstruction (PUSH_BOOL True)
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

    it "executes PUSH_TUPLE" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 3, VInt 2, VInt 1] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction (PUSH_TUPLE 3)
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VTuple [VInt 1, VInt 2, VInt 3]]

  describe "Variable instructions" $ do
    it "executes LOAD_VAR and STORE_VAR" $ do
      state <- createTestVMState
      (result, finalState) <- executeVM state $ do
        executeInstruction (PUSH_INT 42)
        executeInstruction (STORE_VAR (T.pack "x"))
        executeInstruction (LOAD_VAR (T.pack "x"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 42]

    it "executes LOAD_LOCAL and STORE_LOCAL" $ do
      state <- createTestVMState
      (result, finalState) <- executeVM state $ do
        executeInstruction (PUSH_STRING (T.pack "test"))
        executeInstruction (STORE_LOCAL (T.pack "y"))
        executeInstruction (LOAD_LOCAL (T.pack "y"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VString (T.pack "test")]

  describe "Process state instructions" $ do
    it "executes INIT_STATE" $ do
      (result, _) <- runVMWithProcess (Pid 1) $ do
        executeInstruction (PUSH_INT 0)
        executeInstruction INIT_STATE
      result `shouldBe` Right ()

    it "executes GET_STATE and SET_STATE" $ do
      (result, finalState) <- runVMWithProcess (Pid 1) $ do
        executeInstruction (PUSH_INT 42)
        executeInstruction SET_STATE
        executeInstruction GET_STATE
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 42]

  describe "Arithmetic instructions" $ do
    it "executes ADD" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 5, VInt 10] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction ADD
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 15]

    it "executes SUB" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 3, VInt 10] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction SUB
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 7]

    it "executes MUL" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 6, VInt 7] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction MUL
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 42]

    it "executes DIV" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 2, VInt 8] }  -- Stack: top=2, bottom=8
      (result, finalState) <- executeVM stateWithStack $ executeInstruction DIV
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 4]  -- 8 div 2 = 4

    it "returns DivisionByZero error" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 0, VInt 10] }
      (result, _) <- executeVM stateWithStack $ executeInstruction DIV
      result `shouldBe` Left DivisionByZero

    it "executes CONCAT" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VString (T.pack "world"), VString (T.pack "hello")] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction CONCAT
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VString (T.pack "helloworld")]

  describe "Comparison instructions" $ do
    it "executes CMP_EQ for equal values" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 5, VInt 5] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction CMP_EQ
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

    it "executes CMP_EQ for different values" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 3, VInt 5] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction CMP_EQ
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool False]

    it "executes CMP_NEQ" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 3, VInt 5] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction CMP_NEQ
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

    it "executes CMP_LT" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 10, VInt 5] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction CMP_LT
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

    it "executes CMP_GT" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 3, VInt 10] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction CMP_GT
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

    it "executes CMP_LTE" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 5, VInt 5] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction CMP_LTE
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

    it "executes CMP_GTE" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 5, VInt 5] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction CMP_GTE
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

  describe "Logical instructions" $ do
    it "executes LOGIC_AND with true and true" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VBool True, VBool True] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction LOGIC_AND
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

    it "executes LOGIC_AND with true and false" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VBool False, VBool True] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction LOGIC_AND
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool False]

    it "executes LOGIC_OR with false and false" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VBool False, VBool False] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction LOGIC_OR
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool False]

    it "executes LOGIC_OR with true and false" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VBool False, VBool True] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction LOGIC_OR
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

  describe "GET_FIELD instruction" $ do
    it "gets field from tuple by index" $ do
      state <- createTestVMState
      let tuple = VTuple [VInt 1, VInt 2, VInt 3]
      let stateWithStack = state { vmStack = [tuple] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction (GET_FIELD (T.pack "1"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 2]

    it "returns error for invalid field index" $ do
      state <- createTestVMState
      let tuple = VTuple [VInt 1, VInt 2]
      let stateWithStack = state { vmStack = [tuple] }
      (result, _) <- executeVM stateWithStack $ executeInstruction (GET_FIELD (T.pack "5"))
      result `shouldBe` Left (RuntimeError "Invalid field: 5")

    it "returns error for non-tuple value" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 42] }
      (result, _) <- executeVM stateWithStack $ executeInstruction (GET_FIELD (T.pack "0"))
      result `shouldBe` Left (TypeError "GET_FIELD requires tuple, got VInt 42")

  describe "Actor model instructions" $ do
    it "executes DEFINE_PROCESS" $ do
      state <- createTestVMState
      let body = [PUSH_INT 0, HALT]
      (result, finalState) <- executeVM state $
        executeInstruction (DEFINE_PROCESS (T.pack "Counter") [] body)
      result `shouldBe` Right ()
      let defs = vmProcessDefs finalState
      Map.member (T.pack "Counter") defs `shouldBe` True

    it "executes CREATE_INSTANCE" $ do
      state <- createTestVMState
      let pdef = ProcessDef (T.pack "TestProc") [] [HALT]
      let stateWithDef = state { vmProcessDefs = Map.singleton (T.pack "TestProc") pdef }
      (result, finalState) <- executeVM stateWithDef $
        executeInstruction (CREATE_INSTANCE (T.pack "TestProc"))
      case result of
        Right () -> vmStack finalState `shouldSatisfy` (\s -> not (null s))
        Left err -> expectationFailure $ "Expected success, got: " ++ show err

    it "executes SEND" $ do
      state <- createTestVMState
      -- Setup sender and receiver
      mailbox1 <- newTQueueIO
      mailbox2 <- newTQueueIO
      let sender = Process (Pid 1) [] Map.empty VNone mailbox1 0 [] Nothing
      let receiver = Process (Pid 2) [] Map.empty VNone mailbox2 0 [] Nothing
      atomically $ do
        modifyTVar (vmProcesses state) (Map.insert (Pid 1) sender)
        modifyTVar (vmProcesses state) (Map.insert (Pid 2) receiver)

      let stateWithPid = state { vmCurrentPid = Just (Pid 1), vmStack = [VInt 42, VPid 2] }
      (result, _) <- executeVM stateWithPid $ executeInstruction SEND
      result `shouldBe` Right ()

    it "executes WAIT_MESSAGE" $ do
      state <- createTestVMState
      mailbox <- newTQueueIO
      atomically $ writeTQueue mailbox (Message (Pid 2) (VString (T.pack "msg")))
      let process = Process (Pid 1) [] Map.empty VNone mailbox 0 [] Nothing
      atomically $ modifyTVar (vmProcesses state) (Map.insert (Pid 1) process)

      let stateWithPid = state { vmCurrentPid = Just (Pid 1) }
      (result, finalState) <- executeVM stateWithPid $ executeInstruction WAIT_MESSAGE
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VString (T.pack "msg")]

    it "executes SELF" $ do
      (result, finalState) <- runVMWithProcess (Pid 5) $ executeInstruction SELF
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VPid 5]

    it "executes EXIT_PROCESS" $ do
      (result, _) <- runVMWithProcess (Pid 1) $ executeInstruction EXIT_PROCESS
      result `shouldBe` Right ()

  describe "Pattern matching instructions" $ do
    it "executes MATCH_ATOM when pattern matches" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VAtom (T.pack "ok")], vmBytecode = [HALT, HALT, HALT], vmPc = 0 }
      (result, finalState) <- executeVM stateWithStack $
        executeInstruction (MATCH_ATOM (T.pack "ok") 2)
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` []  -- Value consumed
      vmPc finalState `shouldBe` 0  -- No jump

    it "executes MATCH_ATOM when pattern doesn't match" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VAtom (T.pack "error")], vmBytecode = [HALT, HALT, HALT, HALT], vmPc = 0 }
      (result, finalState) <- executeVM stateWithStack $
        executeInstruction (MATCH_ATOM (T.pack "ok") 2)
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VAtom (T.pack "error")]  -- Value not consumed
      vmPc finalState `shouldBe` 2  -- Jumped

    it "executes MATCH_VAR" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 42] }
      (result, finalState) <- executeVM stateWithStack $
        executeInstruction (MATCH_VAR (T.pack "x"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` []
      Map.lookup (T.pack "x") (vmLocals finalState) `shouldBe` Just (VInt 42)

    it "executes MATCH_TUPLE when size matches" $ do
      state <- createTestVMState
      let tuple = VTuple [VInt 1, VInt 2]
      let stateWithStack = state { vmStack = [tuple], vmBytecode = [HALT, HALT, HALT], vmPc = 0 }
      (result, finalState) <- executeVM stateWithStack $
        executeInstruction (MATCH_TUPLE 2 2)
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 1, VInt 2]  -- Elements pushed in reverse
      vmPc finalState `shouldBe` 0  -- No jump

    it "executes MATCH_TUPLE when size doesn't match" $ do
      state <- createTestVMState
      let tuple = VTuple [VInt 1, VInt 2, VInt 3]
      let stateWithStack = state { vmStack = [tuple], vmBytecode = [HALT, HALT, HALT, HALT], vmPc = 0 }
      (result, finalState) <- executeVM stateWithStack $
        executeInstruction (MATCH_TUPLE 2 2)
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VTuple [VInt 1, VInt 2, VInt 3]]  -- Tuple not consumed
      vmPc finalState `shouldBe` 2  -- Jumped

    it "executes MATCH_WILDCARD" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 999] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction MATCH_WILDCARD
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` []  -- Value consumed but not bound

  describe "Control flow instructions" $ do
    it "executes JUMP" $ do
      state <- createTestVMState
      let stateWithCode = state { vmBytecode = [HALT, HALT, HALT, HALT], vmPc = 1 }
      (result, finalState) <- executeVM stateWithCode $ executeInstruction (JUMP 2)
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 3

    it "executes JUMP_IF_FALSE when condition is false" $ do
      state <- createTestVMState
      let stateWithCode = state { vmStack = [VBool False], vmBytecode = [HALT, HALT, HALT, HALT], vmPc = 0 }
      (result, finalState) <- executeVM stateWithCode $ executeInstruction (JUMP_IF_FALSE 2)
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 2

    it "executes JUMP_IF_FALSE when condition is true" $ do
      state <- createTestVMState
      let stateWithCode = state { vmStack = [VBool True], vmBytecode = [HALT, HALT, HALT], vmPc = 0 }
      (result, finalState) <- executeVM stateWithCode $ executeInstruction (JUMP_IF_FALSE 2)
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 0  -- No jump

    it "executes LABEL" $ do
      state <- createTestVMState
      let stateWithCode = state { vmPc = 5 }
      (result, finalState) <- executeVM stateWithCode $
        executeInstruction (LABEL (T.pack "loop"))
      result `shouldBe` Right ()
      Map.lookup (T.pack "loop") (vmLabels finalState) `shouldBe` Just 5

    it "executes CALL" $ do
      state <- createTestVMState
      let stateWithLabel = state
            { vmLabels = Map.singleton (T.pack "func") 10
            , vmBytecode = replicate 15 HALT
            }
      (result, finalState) <- executeVM stateWithLabel $
        executeInstruction (CALL (T.pack "func"))
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 10

    it "executes RETURN" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction RETURN
      result `shouldBe` Right ()

    it "executes HALT" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction HALT
      result `shouldBe` Left (RuntimeError "HALT instruction executed")

  describe "executeBytecode" $ do
    it "executes simple bytecode program" $ do
      state <- createTestVMState
      let code = [PUSH_INT 10, PUSH_INT 20, ADD, HALT]
      (result, _) <- executeVM state $ executeBytecode code
      result `shouldBe` Right (VInt 30)

    it "returns VUnit for empty bytecode with empty stack" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeBytecode []
      result `shouldBe` Right VUnit

    it "executes bytecode with labels" $ do
      state <- createTestVMState
      let code = [LABEL (T.pack "start"), PUSH_INT 42, HALT]
      (result, finalState) <- executeVM state $ executeBytecode code
      result `shouldBe` Right (VInt 42)
      Map.member (T.pack "start") (vmLabels finalState) `shouldBe` True

  describe "registerLabels" $ do
    it "registers all labels in bytecode" $ do
      state <- createTestVMState
      let code = [PUSH_INT 1, LABEL (T.pack "L1"), PUSH_INT 2, LABEL (T.pack "L2"), HALT]
      (result, finalState) <- executeVM state $ registerLabels code 0
      result `shouldBe` Right ()
      Map.lookup (T.pack "L1") (vmLabels finalState) `shouldBe` Just 1
      Map.lookup (T.pack "L2") (vmLabels finalState) `shouldBe` Just 3

  describe "executeLoop" $ do
    it "executes until end of bytecode with HALT" $ do
      state <- createTestVMState
      let code = [PUSH_INT 1, PUSH_INT 2, ADD, HALT]
      let stateWithCode = state { vmBytecode = code, vmPc = 0 }
      (result, finalState) <- executeVM stateWithCode executeLoop
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 3]

    it "stops at HALT instruction" $ do
      state <- createTestVMState
      let code = [PUSH_INT 1, HALT, PUSH_INT 2]
      let stateWithCode = state { vmBytecode = code, vmPc = 0 }
      (result, finalState) <- executeVM stateWithCode executeLoop
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 1]
      vmPc finalState `shouldBe` 1

    it "stops at RETURN instruction" $ do
      state <- createTestVMState
      let code = [PUSH_INT 42, RETURN, PUSH_INT 99]
      let stateWithCode = state { vmBytecode = code, vmPc = 0 }
      (result, finalState) <- executeVM stateWithCode executeLoop
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 42]

  -- Note: The following tests are commented out because they test internal
  -- helper functions that are not exposed or have been refactored
  {-
  describe "readMaybe" $ do
    it "parses valid integer string" $ do
      readMaybe "42" `shouldBe` Just 42
      readMaybe "0" `shouldBe` Just 0
      readMaybe "999" `shouldBe` Just 999

    it "returns Nothing for invalid string" $ do
      readMaybe "abc" `shouldBe` Nothing
      readMaybe "12x" `shouldBe` Nothing
      readMaybe "" `shouldBe` Nothing
  -}

  describe "traceInstruction" $ do
    it "does nothing when trace is disabled" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ traceInstruction (PUSH_INT 42)
      result `shouldBe` Right ()

    it "prints when trace is enabled" $ do
      state <- createTestVMState
      let traceState = state { vmTraceEnabled = True }
      (result, _) <- executeVM traceState $ traceInstruction (PUSH_INT 42)
      result `shouldBe` Right ()

  describe "executeLoop edge cases" $ do
    it "stops at EXIT_PROCESS instruction" $ do
      state <- createTestVMState
      mailbox <- newTQueueIO
      let process = Process (Pid 1) [] Map.empty VNone mailbox 0 [] Nothing
      atomically $ modifyTVar (vmProcesses state) (Map.insert (Pid 1) process)
      let code = [PUSH_INT 42, EXIT_PROCESS, PUSH_INT 99]
      let stateWithCode = state { vmBytecode = code, vmPc = 0, vmCurrentPid = Just (Pid 1) }
      (result, finalState) <- executeVM stateWithCode executeLoop
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 42]

  -- Note: The following tests are commented out because they test internal
  -- helper functions that are not exposed or have been refactored
  {-
  describe "binaryOp" $ do
    it "performs addition with binaryOp" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 3, VInt 7] }
      (result, finalState) <- executeVM stateWithStack $ binaryOp (+) "ADD"
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 10]

  describe "comparisonOp" $ do
    it "compares values for equality" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 5, VInt 5] }
      (result, finalState) <- executeVM stateWithStack $ comparisonOp (==) "CMP_EQ"
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]

  describe "intComparisonOp" $ do
    it "compares integers" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 10, VInt 5] }
      (result, finalState) <- executeVM stateWithStack $ intComparisonOp (<) "CMP_LT"
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VBool True]
  -}

  describe "Complex bytecode programs" $ do
    it "executes complex arithmetic" $ do
      state <- createTestVMState
      let code = [PUSH_INT 5, PUSH_INT 3, MUL, PUSH_INT 2, ADD, HALT]
      (result, _) <- executeVM state $ executeBytecode code
      result `shouldBe` Right (VInt 17)

    it "executes conditional jump" $ do
      state <- createTestVMState
      let code = [PUSH_BOOL False, JUMP_IF_FALSE 2, PUSH_INT 1, HALT, PUSH_INT 2, HALT]
      (result, _) <- executeVM state $ executeBytecode code
      result `shouldBe` Right (VInt 2)

    it "executes with labels" $ do
      state <- createTestVMState
      let code = [LABEL (T.pack "start"), PUSH_INT 99, LABEL (T.pack "end"), HALT]
      (result, finalState) <- executeVM state $ executeBytecode code
      result `shouldBe` Right (VInt 99)
      -- Verify labels were registered
      Map.member (T.pack "start") (vmLabels finalState) `shouldBe` True
      Map.member (T.pack "end") (vmLabels finalState) `shouldBe` True

  describe "Error handling" $ do
    it "handles TypeError in arithmetic operations" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VBool True, VInt 5] }
      (result, _) <- executeVM stateWithStack $ executeInstruction ADD
      case result of
        Left (TypeError _) -> True `shouldBe` True
        _ -> expectationFailure "Expected TypeError"

    it "handles StackUnderflow in binary operations" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 5] }
      (result, _) <- executeVM stateWithStack $ executeInstruction ADD
      result `shouldBe` Left StackUnderflow

    it "handles TypeError in CONCAT" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 42, VString (T.pack "hello")] }
      (result, _) <- executeVM stateWithStack $ executeInstruction CONCAT
      case result of
        Left (TypeError _) -> True `shouldBe` True
        _ -> expectationFailure "Expected TypeError"

    it "handles TypeError in LOGIC_AND" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 5, VBool True] }
      (result, _) <- executeVM stateWithStack $ executeInstruction LOGIC_AND
      case result of
        Left (TypeError _) -> True `shouldBe` True
        _ -> expectationFailure "Expected TypeError"

    it "handles TypeError in LOGIC_OR" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VString (T.pack "x"), VBool False] }
      (result, _) <- executeVM stateWithStack $ executeInstruction LOGIC_OR
      case result of
        Left (TypeError _) -> True `shouldBe` True
        _ -> expectationFailure "Expected TypeError"

    it "handles TypeError in comparison operations" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VString (T.pack "x"), VInt 5] }
      (result, _) <- executeVM stateWithStack $ executeInstruction CMP_LT
      case result of
        Left (TypeError _) -> True `shouldBe` True
        _ -> expectationFailure "Expected TypeError"

    it "handles TypeError in JUMP_IF_FALSE" $ do
      state <- createTestVMState
      let stateWithCode = state { vmStack = [VInt 42], vmBytecode = [HALT, HALT, HALT] }
      (result, _) <- executeVM stateWithCode $ executeInstruction (JUMP_IF_FALSE 2)
      case result of
        Left (TypeError _) -> True `shouldBe` True
        _ -> expectationFailure "Expected TypeError"

    it "handles invalid field name in GET_FIELD" $ do
      state <- createTestVMState
      let tuple = VTuple [VInt 1, VInt 2]
      let stateWithStack = state { vmStack = [tuple] }
      (result, _) <- executeVM stateWithStack $ executeInstruction (GET_FIELD (T.pack "notanumber"))
      case result of
        Left (RuntimeError _) -> True `shouldBe` True
        _ -> expectationFailure "Expected RuntimeError"

    it "handles StackUnderflow in PUSH_TUPLE" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 1] }
      (result, _) <- executeVM stateWithStack $ executeInstruction (PUSH_TUPLE 5)
      result `shouldBe` Left StackUnderflow

    it "handles UndefinedVariable in LOAD_VAR" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction (LOAD_VAR (T.pack "undefined"))
      result `shouldBe` Left (UndefinedVariable (T.pack "undefined"))

    it "handles UndefinedVariable in LOAD_LOCAL" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction (LOAD_LOCAL (T.pack "undefined"))
      result `shouldBe` Left (UndefinedVariable (T.pack "undefined"))

    it "handles InvalidLabel in CALL" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction (CALL (T.pack "nonexistent"))
      result `shouldBe` Left (InvalidLabel (T.pack "nonexistent"))

    -- Additional error handling tests for better boolean coverage
    it "handles TypeError in INC_VAR with non-integer" $ do
      state <- createTestVMState
      let stateWithLocals = state { vmLocals = Map.fromList [(T.pack "x", VString (T.pack "not_int"))] }
      (result, _) <- executeVM stateWithLocals $ executeInstruction (INC_VAR (T.pack "x"))
      result `shouldBe` Left (TypeError "INC_VAR requires integer")

    it "handles TypeError in DEC_VAR with non-integer" $ do
      state <- createTestVMState
      let stateWithLocals = state { vmLocals = Map.fromList [(T.pack "x", VBool True)] }
      (result, _) <- executeVM stateWithLocals $ executeInstruction (DEC_VAR (T.pack "x"))
      result `shouldBe` Left (TypeError "DEC_VAR requires integer")

    it "handles TypeError in INC_VAR_POST with non-integer" $ do
      state <- createTestVMState
      let stateWithLocals = state { vmLocals = Map.fromList [(T.pack "x", VUnit)] }
      (result, _) <- executeVM stateWithLocals $ executeInstruction (INC_VAR_POST (T.pack "x"))
      result `shouldBe` Left (TypeError "INC_VAR_POST requires integer")

    it "handles TypeError in DEC_VAR_POST with non-integer" $ do
      state <- createTestVMState
      let stateWithLocals = state { vmLocals = Map.fromList [(T.pack "x", VNone)] }
      (result, _) <- executeVM stateWithLocals $ executeInstruction (DEC_VAR_POST (T.pack "x"))
      result `shouldBe` Left (TypeError "DEC_VAR_POST requires integer")

    it "handles TypeError in MAYBE_BIND with non-Maybe value" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VInt 42] }
      (result, _) <- executeVM stateWithStack $ executeInstruction (MAYBE_BIND (T.pack "func"))
      result `shouldBe` Left (TypeError "MAYBE_BIND requires Maybe value")

    it "handles TypeError in EITHER_BIND with non-Either value" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VString (T.pack "test")] }
      (result, _) <- executeVM stateWithStack $ executeInstruction (EITHER_BIND (T.pack "func"))
      result `shouldBe` Left (TypeError "EITHER_BIND requires Either value")

    it "handles RuntimeError for PUSH_FLOAT (not implemented)" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction (PUSH_FLOAT 3.14)
      result `shouldBe` Left (RuntimeError "PUSH_FLOAT not yet implemented in VM")

    it "handles RuntimeError for PUSH_ARRAY (not implemented)" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction (PUSH_ARRAY 5)
      result `shouldBe` Left (RuntimeError "PUSH_ARRAY not yet implemented in VM")

    it "handles RuntimeError for INDEX (not implemented)" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction INDEX
      result `shouldBe` Left (RuntimeError "INDEX not yet implemented in VM")

    it "handles RuntimeError for ARRAY_LENGTH (not implemented)" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction ARRAY_LENGTH
      result `shouldBe` Left (RuntimeError "ARRAY_LENGTH not yet implemented in VM")

    it "handles RuntimeError for STATIC_CAST (not implemented)" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction (STATIC_CAST (T.pack "Int"))
      result `shouldBe` Left (RuntimeError "STATIC_CAST not yet implemented in VM")

    it "handles RuntimeError for REINTERPRET_CAST (not implemented)" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction (REINTERPRET_CAST (T.pack "Float"))
      result `shouldBe` Left (RuntimeError "REINTERPRET_CAST not yet implemented in VM")

    it "handles RuntimeError for CONST_CAST (not implemented)" $ do
      state <- createTestVMState
      (result, _) <- executeVM state $ executeInstruction CONST_CAST
      result `shouldBe` Left (RuntimeError "CONST_CAST not yet implemented in VM")

    -- Additional tests for executeLoop edge cases
    it "executeLoop stops when pc >= length bytecode" $ do
      state <- createTestVMState
      let bytecode = [PUSH_INT 1, PUSH_INT 2]
      let stateWithBytecode = state { vmBytecode = bytecode, vmPc = 2 }  -- pc at end
      (result, finalState) <- executeVM stateWithBytecode executeLoop
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 2

    -- Additional pattern matching tests for full coverage
    it "executes MATCH_ATOM with different atom types" $ do
      state <- createTestVMState
      let bytecode = replicate 6 HALT  -- Enough for jump to 5
      let stateWithStack = state { vmStack = [VAtom (T.pack "different")], vmBytecode = bytecode }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction (MATCH_ATOM (T.pack "match") 5)
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 5  -- Should jump because atoms don't match

    it "executes MATCH_TUPLE with wrong size and jumps" $ do
      state <- createTestVMState
      let bytecode = replicate 11 HALT  -- Enough for jump to 10
      let stateWithStack = state { vmStack = [VTuple [VInt 1, VInt 2]], vmBytecode = bytecode }  -- Size 2
      (result, finalState) <- executeVM stateWithStack $ executeInstruction (MATCH_TUPLE 3 10)  -- Expecting size 3
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 10  -- Should jump because sizes don't match

    -- Additional GET_FIELD tests for edge cases
    it "handles GET_FIELD with negative index string" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VTuple [VInt 1, VInt 2]] }
      (result, _) <- executeVM stateWithStack $ executeInstruction (GET_FIELD (T.pack "-1"))
      result `shouldBe` Left (RuntimeError "Invalid field: -1")

    it "handles GET_FIELD with non-numeric field name" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VTuple [VInt 1, VInt 2]] }
      (result, _) <- executeVM stateWithStack $ executeInstruction (GET_FIELD (T.pack "abc"))
      result `shouldBe` Left (RuntimeError "Invalid field: abc")

    -- Test for MAYBE_BIND with VNone (propagates None)
    it "executes MAYBE_BIND with VNone (propagates None)" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VNone] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction (MAYBE_BIND (T.pack "func"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VNone]

    -- Test for EITHER_BIND with VLeft (propagates Left)
    it "executes EITHER_BIND with VLeft (propagates Left)" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VLeft (VString (T.pack "error"))] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction (EITHER_BIND (T.pack "func"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VLeft (VString (T.pack "error"))]

    -- Test for EITHER_BIND with VRight (unwraps and jumps)
    it "executes EITHER_BIND with VRight (unwraps and jumps)" $ do
      let bytecode = replicate 11 HALT
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VRight (VInt 42)], vmBytecode = bytecode }
      (result, _finalState) <- executeVM stateWithStack $ executeInstruction (EITHER_BIND (T.pack "func"))
      result `shouldBe` Left (RuntimeError "Label func not found")

    -- Test for MAYBE_BIND with VJust (unwraps and jumps)
    it "executes MAYBE_BIND with VJust (unwraps and jumps)" $ do
      let bytecode = replicate 11 HALT  -- Enough for jump
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VJust (VInt 42)], vmBytecode = bytecode }
      (result, _finalState) <- executeVM stateWithStack $ executeInstruction (MAYBE_BIND (T.pack "func"))
      result `shouldBe` Left (RuntimeError "Label func not found")  -- Label not found

    it "registers all labels in bytecode" $ do
      state <- createTestVMState
      let code = [PUSH_INT 1, LABEL (T.pack "L1"), PUSH_INT 2, LABEL (T.pack "L2"), HALT]
      (result, finalState) <- executeVM state $ registerLabels code 0
      result `shouldBe` Right ()
      Map.lookup (T.pack "L1") (vmLabels finalState) `shouldBe` Just 1
      Map.lookup (T.pack "L2") (vmLabels finalState) `shouldBe` Just 3

    -- Test for executeLoop with RETURN instruction
    it "executeLoop stops at RETURN instruction" $ do
      let bytecode = [PUSH_INT 1, RETURN, PUSH_INT 2]  -- Should stop at RETURN
      state <- createTestVMState
      let stateWithBytecode = state { vmBytecode = bytecode }
      (result, finalState) <- executeVM stateWithBytecode executeLoop
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 1]  -- Only first instruction executed
      vmPc finalState `shouldBe` 1

    -- Test for executeLoop with EXIT_PROCESS instruction
    it "executeLoop stops at EXIT_PROCESS instruction" $ do
      let bytecode = [PUSH_INT 1, EXIT_PROCESS, PUSH_INT 2]  -- Should stop at EXIT_PROCESS
      state <- createTestVMState
      let stateWithBytecode = state { vmBytecode = bytecode }
      (result, finalState) <- executeVM stateWithBytecode executeLoop
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 1]  -- Only first instruction executed
      vmPc finalState `shouldBe` 1

    -- Test for traceInstruction with trace disabled (already exists)
    -- Test for traceInstruction with trace enabled (already exists)

    -- Test for readMaybe helper (internal function, test indirectly through GET_FIELD)
    it "GET_FIELD handles valid numeric field names" $ do
      state <- createTestVMState
      let stateWithStack = state { vmStack = [VTuple [VInt 10, VInt 20, VInt 30]] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction (GET_FIELD (T.pack "1"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 20]  -- 0-indexed, so "1" gives second element

{-
-- Real tests for unimplemented features (when implemented):

it "executes PUSH_FLOAT" $ do
  state <- createTestVMState
  (result, finalState) <- executeVM state $ executeInstruction (PUSH_FLOAT 3.14)
  result `shouldBe` Right ()
  vmStack finalState `shouldBe` [VFloat 3.14]

it "executes PUSH_ARRAY with elements" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VInt 1, VInt 2, VInt 3] }
  (result, finalState) <- executeVM stateWithStack $ executeInstruction (PUSH_ARRAY 3)
  result `shouldBe` Right ()
  vmStack finalState `shouldBe` [VArray [VInt 1, VInt 2, VInt 3]]

it "executes PUSH_ARRAY with zero elements" $ do
  state <- createTestVMState
  (result, finalState) <- executeVM state $ executeInstruction (PUSH_ARRAY 0)
  result `shouldBe` Right ()
  vmStack finalState `shouldBe` [VArray []]

it "executes INDEX on array" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VArray [VInt 10, VInt 20, VInt 30], VInt 1] }
  (result, finalState) <- executeVM stateWithStack $ executeInstruction INDEX
  result `shouldBe` Right ()
  vmStack finalState `shouldBe` [VInt 20]

it "executes ARRAY_LENGTH" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VArray [VInt 1, VInt 2, VInt 3, VInt 4]] }
  (result, finalState) <- executeVM stateWithStack $ executeInstruction ARRAY_LENGTH
  result `shouldBe` Right ()
  vmStack finalState `shouldBe` [VInt 4]

it "executes STATIC_CAST to int" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VFloat 3.14] }
  (result, finalState) <- executeVM stateWithStack $ executeInstruction (STATIC_CAST (pack "i32"))
  result `shouldBe` Right ()
  vmStack finalState `shouldBe` [VInt 3]  -- Truncated float to int

it "executes REINTERPRET_CAST" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VInt 42] }
  (result, finalState) <- executeVM stateWithStack $ executeInstruction (REINTERPRET_CAST (pack "float"))
  result `shouldBe` Right ()
  vmStack finalState `shouldBe` [VFloat 42.0]  -- Reinterpreted bits

it "executes CONST_CAST (no-op for now)" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VInt 100] }
  (result, finalState) <- executeVM stateWithStack $ executeInstruction CONST_CAST
  result `shouldBe` Right ()
  vmStack finalState `shouldBe` [VInt 100]  -- No change

-- Error cases for new features:

it "handles StackUnderflow in PUSH_ARRAY" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VInt 1] }  -- Not enough elements
  (result, _) <- executeVM stateWithStack $ executeInstruction (PUSH_ARRAY 3)
  result `shouldBe` Left StackUnderflow

it "handles TypeError in INDEX with non-array" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VInt 42, VInt 0] }
  (result, _) <- executeVM stateWithStack $ executeInstruction INDEX
  result `shouldBe` Left (TypeError "INDEX requires array")

it "handles TypeError in INDEX with non-integer index" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VArray [VInt 1], VString (pack "not_int")] }
  (result, _) <- executeVM stateWithStack $ executeInstruction INDEX
  result `shouldBe` Left (TypeError "INDEX requires integer index")

it "handles RuntimeError in INDEX with out of bounds" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VArray [VInt 1, VInt 2], VInt 5] }
  (result, _) <- executeVM stateWithStack $ executeInstruction INDEX
  result `shouldBe` Left (RuntimeError "Array index out of bounds")

it "handles TypeError in ARRAY_LENGTH with non-array" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VInt 42] }
  (result, _) <- executeVM stateWithStack $ executeInstruction ARRAY_LENGTH
  result `shouldBe` Left (TypeError "ARRAY_LENGTH requires array")

it "handles TypeError in STATIC_CAST with invalid cast" $ do
  state <- createTestVMState
  let stateWithStack = state { vmStack = [VString (pack "not_a_number")] }
  (result, _) <- executeVM stateWithStack $ executeInstruction (STATIC_CAST (pack "i32"))
  result `shouldBe` Left (TypeError "Cannot cast string to i32")

-}
