{-
-- EPITECH PROJECT, 2025
-- glados-vm
-- File description:
-- VM module tests
-}

module VMSpec (spec) where

import Test.Hspec
import Ratatouille.VM.VM
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

-- Helper to run VM and extract result
runVMTest :: VM a -> IO (Either VMError a, VMState)
runVMTest action = do
  state <- createTestVMState
  executeVM state action

spec :: Spec
spec = do
  describe "VMError" $ do
    it "shows StackUnderflow" $ do
      show StackUnderflow `shouldBe` "StackUnderflow"

    it "shows TypeError" $ do
      show (TypeError "test error") `shouldBe` "TypeError \"test error\""

    it "shows UndefinedVariable" $ do
      show (UndefinedVariable (T.pack "x")) `shouldBe` "UndefinedVariable \"x\""

    it "shows UndefinedProcess" $ do
      show (UndefinedProcess (T.pack "MyProc")) `shouldBe` "UndefinedProcess \"MyProc\""

    it "shows InvalidJump" $ do
      show (InvalidJump 42) `shouldBe` "InvalidJump 42"

    it "shows InvalidLabel" $ do
      show (InvalidLabel (T.pack "loop")) `shouldBe` "InvalidLabel \"loop\""

    it "shows DivisionByZero" $ do
      show DivisionByZero `shouldBe` "DivisionByZero"

    it "shows ProcessError" $ do
      show (ProcessError (T.pack "error")) `shouldBe` "ProcessError \"error\""

    it "shows PatternMatchFailed" $ do
      show PatternMatchFailed `shouldBe` "PatternMatchFailed"

    it "shows RuntimeError" $ do
      show (RuntimeError "runtime error") `shouldBe` "RuntimeError \"runtime error\""

  describe "Pid" $ do
    it "shows Pid correctly" $ do
      show (Pid 42) `shouldBe` "Pid 42"

    it "compares Pids for equality" $ do
      Pid 1 `shouldBe` Pid 1
      Pid 1 `shouldNotBe` Pid 2

    it "orders Pids correctly" $ do
      Pid 1 < Pid 2 `shouldBe` True
      Pid 3 > Pid 1 `shouldBe` True

    it "supports numeric operations" $ do
      Pid 5 + Pid 3 `shouldBe` Pid 8
      Pid 10 - Pid 3 `shouldBe` Pid 7

  describe "Message" $ do
    it "creates and shows messages" $ do
      let msg = Message { msgSender = Pid 1, msgContent = VInt 42 }
      msgSender msg `shouldBe` Pid 1
      msgContent msg `shouldBe` VInt 42

    it "compares messages for equality" $ do
      let msg1 = Message (Pid 1) (VInt 42)
      let msg2 = Message (Pid 1) (VInt 42)
      let msg3 = Message (Pid 2) (VInt 42)
      msg1 `shouldBe` msg2
      msg1 `shouldNotBe` msg3

  describe "ProcessDef" $ do
    it "creates process definition" $ do
      let pdef = ProcessDef (T.pack "MyProc") [T.pack "x", T.pack "y"] [PUSH_INT 1, HALT]
      procName pdef `shouldBe` T.pack "MyProc"
      procParams pdef `shouldBe` [T.pack "x", T.pack "y"]
      procBody pdef `shouldBe` [PUSH_INT 1, HALT]

    it "compares process definitions for equality" $ do
      let pdef1 = ProcessDef (T.pack "P") [] [HALT]
      let pdef2 = ProcessDef (T.pack "P") [] [HALT]
      pdef1 `shouldBe` pdef2

  describe "Stack operations" $ do
    it "pushes values onto stack" $ do
      (result, finalState) <- runVMTest $ do
        pushStack (VInt 42)
        pushStack (VInt 10)
        return ()
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 10, VInt 42]

    it "pops values from stack" $ do
      (result, finalState) <- runVMTest $ do
        pushStack (VInt 42)
        pushStack (VInt 10)
        val <- popStack
        return val
      result `shouldBe` Right (VInt 10)
      vmStack finalState `shouldBe` [VInt 42]

    it "returns StackUnderflow when popping empty stack" $ do
      (result, _) <- runVMTest popStack
      result `shouldBe` Left StackUnderflow

    it "peeks at top of stack without removing" $ do
      (result, finalState) <- runVMTest $ do
        pushStack (VInt 42)
        val <- peekStack
        return val
      result `shouldBe` Right (VInt 42)
      vmStack finalState `shouldBe` [VInt 42]

    it "returns StackUnderflow when peeking empty stack" $ do
      (result, _) <- runVMTest peekStack
      result `shouldBe` Left StackUnderflow

    it "pops N values from stack" $ do
      (result, finalState) <- runVMTest $ do
        pushStack (VInt 1)
        pushStack (VInt 2)
        pushStack (VInt 3)
        vals <- popStackN 2
        return vals
      result `shouldBe` Right [VInt 3, VInt 2]
      vmStack finalState `shouldBe` [VInt 1]

    it "returns StackUnderflow when popping too many values" $ do
      (result, _) <- runVMTest $ do
        pushStack (VInt 1)
        popStackN 2
      result `shouldBe` Left StackUnderflow

  describe "Variable operations" $ do
    it "stores and loads global variables" $ do
      (result, _) <- runVMTest $ do
        storeGlobal (T.pack "x") (VInt 42)
        loadGlobal (T.pack "x")
      result `shouldBe` Right (VInt 42)

    it "returns UndefinedVariable for missing global" $ do
      (result, _) <- runVMTest $ loadGlobal (T.pack "missing")
      result `shouldBe` Left (UndefinedVariable (T.pack "missing"))

    it "stores and loads local variables" $ do
      (result, _) <- runVMTest $ do
        storeLocal (T.pack "y") (VString (T.pack "hello"))
        loadLocal (T.pack "y")
      result `shouldBe` Right (VString (T.pack "hello"))

    it "returns UndefinedVariable for missing local" $ do
      (result, _) <- runVMTest $ loadLocal (T.pack "missing")
      result `shouldBe` Left (UndefinedVariable (T.pack "missing"))

    it "overwrites existing variables" $ do
      (result, _) <- runVMTest $ do
        storeGlobal (T.pack "x") (VInt 1)
        storeGlobal (T.pack "x") (VInt 2)
        loadGlobal (T.pack "x")
      result `shouldBe` Right (VInt 2)

  describe "PC operations" $ do
    it "gets and sets program counter" $ do
      (result, finalState) <- runVMTest $ do
        setPc 42
        getPc
      result `shouldBe` Right 42
      vmPc finalState `shouldBe` 42

    it "increments program counter" $ do
      (result, finalState) <- runVMTest $ do
        setPc 10
        incrementPc
        getPc
      result `shouldBe` Right 11
      vmPc finalState `shouldBe` 11

    it "jumps by offset" $ do
      state <- createTestVMState
      let stateWithCode = state { vmBytecode = [PUSH_INT 1, PUSH_INT 2, PUSH_INT 3, HALT], vmPc = 1 }
      (result, finalState) <- executeVM stateWithCode $ jump 2
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 3

    it "returns InvalidJump for negative jump" $ do
      state <- createTestVMState
      let stateWithCode = state { vmBytecode = [PUSH_INT 1, HALT], vmPc = 1 }
      (result, _) <- executeVM stateWithCode $ jump (-5)
      result `shouldBe` Left (InvalidJump (-4))

    it "returns InvalidJump for jump beyond bytecode" $ do
      state <- createTestVMState
      let stateWithCode = state { vmBytecode = [PUSH_INT 1, HALT], vmPc = 0 }
      (result, _) <- executeVM stateWithCode $ jump 10
      result `shouldBe` Left (InvalidJump 10)

    it "jumps to absolute position" $ do
      state <- createTestVMState
      let stateWithCode = state { vmBytecode = [PUSH_INT 1, PUSH_INT 2, PUSH_INT 3, HALT] }
      (result, finalState) <- executeVM stateWithCode $ jumpTo 2
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 2

    it "returns InvalidJump for invalid absolute jump" $ do
      state <- createTestVMState
      let stateWithCode = state { vmBytecode = [PUSH_INT 1, HALT] }
      (result, _) <- executeVM stateWithCode $ jumpTo 10
      result `shouldBe` Left (InvalidJump 10)

  describe "Label operations" $ do
    it "registers and finds labels" $ do
      (result, _) <- runVMTest $ do
        registerLabel (T.pack "loop") 10
        findLabel (T.pack "loop")
      result `shouldBe` Right 10

    it "returns InvalidLabel for missing label" $ do
      (result, _) <- runVMTest $ findLabel (T.pack "missing")
      result `shouldBe` Left (InvalidLabel (T.pack "missing"))

    it "overwrites existing labels" $ do
      (result, _) <- runVMTest $ do
        registerLabel (T.pack "start") 5
        registerLabel (T.pack "start") 10
        findLabel (T.pack "start")
      result `shouldBe` Right 10

  describe "Process definition operations" $ do
    it "defines and gets process definitions" $ do
      (result, _) <- runVMTest $ do
        let pdef = ProcessDef (T.pack "Counter") [] [PUSH_INT 0, HALT]
        defineProcess pdef
        getProcessDef (T.pack "Counter")
      result `shouldBe` Right (ProcessDef (T.pack "Counter") [] [PUSH_INT 0, HALT])

    it "returns UndefinedProcess for missing process" $ do
      (result, _) <- runVMTest $ getProcessDef (T.pack "Missing")
      result `shouldBe` Left (UndefinedProcess (T.pack "Missing"))

  describe "Debug operations" $ do
    it "checks debug mode" $ do
      state <- createTestVMState
      let debugState = state { vmDebugMode = True }
      (result, _) <- executeVM debugState isDebugMode
      result `shouldBe` Right True

    it "checks trace enabled" $ do
      state <- createTestVMState
      let traceState = state { vmTraceEnabled = True }
      (result, _) <- executeVM traceState isTraceEnabled
      result `shouldBe` Right True

    it "checks breakpoint" $ do
      state <- createTestVMState
      let bpState = state { vmPc = 5, vmBreakpoints = [3, 5, 7] }
      (result, _) <- executeVM bpState checkBreakpoint
      result `shouldBe` Right True

    it "returns False when not at breakpoint" $ do
      state <- createTestVMState
      let bpState = state { vmPc = 4, vmBreakpoints = [3, 5, 7] }
      (result, _) <- executeVM bpState checkBreakpoint
      result `shouldBe` Right False

  describe "Helper functions" $ do
    it "converts VBool to Bool" $ do
      (result, _) <- runVMTest $ toBool (VBool True)
      result `shouldBe` Right True

    it "returns TypeError for non-Bool in toBool" $ do
      (result, _) <- runVMTest $ toBool (VInt 42)
      result `shouldBe` Left (TypeError "Expected Bool, got VInt 42")

    it "converts VInt to Integer" $ do
      (result, _) <- runVMTest $ toInt (VInt 42)
      result `shouldBe` Right 42

    it "returns TypeError for non-Int in toInt" $ do
      (result, _) <- runVMTest $ toInt (VBool True)
      result `shouldBe` Left (TypeError "Expected Int, got VBool True")

    it "converts VString to Text" $ do
      (result, _) <- runVMTest $ toString (VString (T.pack "hello"))
      result `shouldBe` Right (T.pack "hello")

    it "returns TypeError for non-String in toString" $ do
      (result, _) <- runVMTest $ toString (VInt 42)
      result `shouldBe` Left (TypeError "Expected String, got VInt 42")

    it "converts VTuple to list" $ do
      (result, _) <- runVMTest $ toTuple (VTuple [VInt 1, VInt 2])
      result `shouldBe` Right [VInt 1, VInt 2]

    it "returns TypeError for non-Tuple in toTuple" $ do
      (result, _) <- runVMTest $ toTuple (VInt 42)
      result `shouldBe` Left (TypeError "Expected Tuple, got VInt 42")

    it "converts VPid to Pid" $ do
      (result, _) <- runVMTest $ toPid (VPid 42)
      result `shouldBe` Right (Pid 42)

    it "returns TypeError for non-Pid in toPid" $ do
      (result, _) <- runVMTest $ toPid (VInt 42)
      result `shouldBe` Left (TypeError "Expected Pid, got VInt 42")

  describe "Process Show instance" $ do
    it "shows Process details" $ do
      mailbox <- newTQueueIO
      let process = Process
            { processId = Pid 42
            , processStack = [VInt 1, VInt 2]
            , processLocals = Map.singleton (T.pack "x") (VInt 10)
            , processState = VNone
            , processMailbox = mailbox
            , processPc = 5
            , processBytecode = [PUSH_INT 1]
            , processThreadId = Nothing
            }
      let output = show process
      output `shouldContain` "Process{pid=Pid 42"
      output `shouldContain` "stack="
      output `shouldContain` "pc=5"

  describe "VMState operations with breakpoints" $ do
    it "handles multiple breakpoints" $ do
      state <- createTestVMState
      let bpState = state { vmPc = 10, vmBreakpoints = [5, 10, 15, 20] }
      (result, _) <- executeVM bpState checkBreakpoint
      result `shouldBe` Right True

    it "checks breakpoint with empty list" $ do
      state <- createTestVMState
      let bpState = state { vmPc = 5, vmBreakpoints = [] }
      (result, _) <- executeVM bpState checkBreakpoint
      result `shouldBe` Right False

  describe "Multiple stack operations" $ do
    it "handles complex stack manipulations" $ do
      (result, finalState) <- runVMTest $ do
        pushStack (VInt 1)
        pushStack (VInt 2)
        pushStack (VInt 3)
        _ <- popStack
        val <- peekStack
        pushStack (VInt 4)
        return val
      result `shouldBe` Right (VInt 2)
      vmStack finalState `shouldBe` [VInt 4, VInt 2, VInt 1]

  describe "Global and local variable interaction" $ do
    it "handles same name in global and local scope" $ do
      (result, finalState) <- runVMTest $ do
        storeGlobal (T.pack "x") (VInt 100)
        storeLocal (T.pack "x") (VInt 200)
        globalVal <- loadGlobal (T.pack "x")
        localVal <- loadLocal (T.pack "x")
        return (globalVal, localVal)
      result `shouldBe` Right (VInt 100, VInt 200)

  describe "PC edge cases" $ do
    it "handles PC at last bytecode position" $ do
      state <- createTestVMState
      let stateWithCode = state { vmBytecode = [PUSH_INT 1, PUSH_INT 2, HALT], vmPc = 1 }
      (result, finalState) <- executeVM stateWithCode $ jump 1
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 2

    it "handles PC at zero after jump" $ do
      state <- createTestVMState
      let stateWithCode = state { vmBytecode = [PUSH_INT 1, PUSH_INT 2], vmPc = 1 }
      (result, finalState) <- executeVM stateWithCode $ jump (-1)
      result `shouldBe` Right ()
      vmPc finalState `shouldBe` 0

  describe "Label operations with multiple labels" $ do
    it "handles multiple labels" $ do
      (result, finalState) <- runVMTest $ do
        registerLabel (T.pack "start") 0
        registerLabel (T.pack "loop") 10
        registerLabel (T.pack "end") 20
        pc1 <- findLabel (T.pack "start")
        pc2 <- findLabel (T.pack "loop")
        pc3 <- findLabel (T.pack "end")
        return (pc1, pc2, pc3)
      result `shouldBe` Right (0, 10, 20)

  describe "VMError equality" $ do
    it "compares VMErrors correctly" $ do
      StackUnderflow `shouldBe` StackUnderflow
      DivisionByZero `shouldBe` DivisionByZero
      PatternMatchFailed `shouldBe` PatternMatchFailed
      TypeError "test" `shouldBe` TypeError "test"
      TypeError "test" `shouldNotBe` TypeError "other"
