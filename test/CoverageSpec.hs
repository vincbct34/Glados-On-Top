{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Additional tests to improve code coverage
-}

module CoverageSpec (spec) where

import Test.Hspec
import Ratatouille.VM.VM
import Ratatouille.VM.Runtime ()
import Ratatouille.VM.Interpreter (executeInstruction, valueToString)
import Ratatouille.Bytecode.Types
import Ratatouille.Bytecode.Compiler (compileExpr, compileStmt, compilePattern)
import Ratatouille.AST
import Control.Concurrent.STM
import qualified Data.Map as Map
import qualified Data.Text as T ()
import Data.Text (pack)

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
    , vmFunctionDefs = Map.empty
    }

spec :: Spec
spec = describe "Coverage Tests" $ do
  
  describe "valueToString function" $ do
    it "handles VFloat values" $ do
      let result = valueToString (VFloat 3.14)
      result `shouldBe` "3.14"

    it "handles VArray values" $ do
      let result = valueToString (VArray [VInt 1, VInt 2, VInt 3])
      result `shouldBe` "[1, 2, 3]"

    it "handles VJust values" $ do
      let result = valueToString (VJust (VString (pack "hello")))
      result `shouldBe` "Just hello"

    it "handles VLeft values" $ do
      let result = valueToString (VLeft (VString (pack "error")))
      result `shouldBe` "Left error"

    it "handles VRight values" $ do
      let result = valueToString (VRight (VBool True))
      result `shouldBe` "Right true"

    it "handles empty VArray" $ do
      let result = valueToString (VArray [])
      result `shouldBe` "[]"

    it "handles nested VTuple" $ do
      let result = valueToString (VTuple [VInt 1, VTuple [VInt 2, VInt 3]])
      result `shouldBe` "(1, (2, 3))"

  describe "Additional instruction coverage" $ do
    it "handles INC_VAR instruction" $ do
      state <- createTestVMState
      let stateWithLocal = state { vmLocals = Map.singleton (pack "counter") (VInt 5) }
      (result, finalState) <- executeVM stateWithLocal $ executeInstruction (INC_VAR (pack "counter"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 6]

    it "handles DEC_VAR instruction" $ do
      state <- createTestVMState
      let stateWithLocal = state { vmLocals = Map.singleton (pack "counter") (VInt 5) }
      (result, finalState) <- executeVM stateWithLocal $ executeInstruction (DEC_VAR (pack "counter"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 4]

    it "handles INC_VAR_POST instruction" $ do
      state <- createTestVMState
      let stateWithLocal = state { vmLocals = Map.singleton (pack "counter") (VInt 5) }
      (result, finalState) <- executeVM stateWithLocal $ executeInstruction (INC_VAR_POST (pack "counter"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 5] -- Old value pushed

    it "handles DEC_VAR_POST instruction" $ do
      state <- createTestVMState
      let stateWithLocal = state { vmLocals = Map.singleton (pack "counter") (VInt 5) }
      (result, finalState) <- executeVM stateWithLocal $ executeInstruction (DEC_VAR_POST (pack "counter"))
      result `shouldBe` Right ()
      vmStack finalState `shouldBe` [VInt 5] -- Old value pushed

  describe "Compiler edge cases" $ do
    it "compiles EPreInc expression" $ do
      let expr = EPreInc (pack "x")
      let bytecode = compileExpr expr
      bytecode `shouldBe` [INC_VAR (pack "x")]

    it "compiles EPostInc expression" $ do
      let expr = EPostInc (pack "x")
      let bytecode = compileExpr expr
      bytecode `shouldBe` [INC_VAR_POST (pack "x")]

    it "compiles EPreDec expression" $ do
      let expr = EPreDec (pack "x")
      let bytecode = compileExpr expr
      bytecode `shouldBe` [DEC_VAR (pack "x")]

    it "compiles EPostDec expression" $ do
      let expr = EPostDec (pack "x")
      let bytecode = compileExpr expr
      bytecode `shouldBe` [DEC_VAR_POST (pack "x")]

    it "compiles ENone expression" $ do
      let expr = ENone
      let bytecode = compileExpr expr
      bytecode `shouldBe` [PUSH_NONE]

    it "compiles ELeft expression" $ do
      let expr = ELeft (ELiteral (LString (pack "error")))
      let bytecode = compileExpr expr
      bytecode `shouldBe` [PUSH_STRING (pack "error"), PUSH_LEFT]

    it "compiles ERight expression" $ do
      let expr = ERight (ELiteral (LInt 42))
      let bytecode = compileExpr expr
      bytecode `shouldBe` [PUSH_INT 42, PUSH_RIGHT]

    it "compiles EJust expression" $ do
      let expr = EJust (ELiteral (LString (pack "value")))
      let bytecode = compileExpr expr
      bytecode `shouldBe` [PUSH_STRING (pack "value"), PUSH_JUST]

  describe "Pattern matching edge cases" $ do
    it "compiles PWildcard pattern" $ do
      let pattern = PWildcard
      let bytecode = compilePattern pattern
      bytecode `shouldBe` [MATCH_WILDCARD]

    it "compiles PVarTyped pattern" $ do
      let pattern = PVarTyped (pack "x") Nothing False
      let bytecode = compilePattern pattern
      bytecode `shouldBe` [MATCH_VAR (pack "x")]

    it "compiles PVarargs pattern" $ do
      let pattern = PVarargs (pack "rest")
      let bytecode = compilePattern pattern
      bytecode `shouldBe` [MATCH_VAR (pack "rest")]

  describe "Statement compilation edge cases" $ do
    it "compiles SConst statement" $ do
      let stmt = SConst (pack "PI") Nothing (ELiteral (LFloat 3.14))
      let bytecode = compileStmt stmt
      bytecode `shouldBe` [PUSH_FLOAT 3.14, STORE_LOCAL (pack "PI")]

    it "compiles SLetPattern statement" $ do
      let stmt = SLetPattern (PVar (pack "x")) (ELiteral (LInt 42))
      let bytecode = compileStmt stmt
      bytecode `shouldBe` [PUSH_INT 42, STORE_LOCAL (pack "x")]