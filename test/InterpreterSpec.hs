{-
-- EPITECH PROJECT, 2025
-- glados-vm
-- File description:
-- Interpreter module tests
-}

module InterpreterSpec (spec) where

import Test.Hspec
import Ratatouille.VM.VM
import Ratatouille.VM.Runtime
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
      let stateWithStack = state { vmStack = [VInt 5, VInt 20] }
      (result, finalState) <- executeVM stateWithStack $ executeInstruction DIV
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 4]

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
      (result, _) <- executeVM state $ executeBytecode [HALT]
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

  describe "readMaybe" $ do
    it "parses valid integer string" $ do
      readMaybe "42" `shouldBe` Just 42
      readMaybe "0" `shouldBe` Just 0
      readMaybe "999" `shouldBe` Just 999

    it "returns Nothing for invalid string" $ do
      readMaybe "abc" `shouldBe` Nothing
      readMaybe "12x" `shouldBe` Nothing
      readMaybe "" `shouldBe` Nothing

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
