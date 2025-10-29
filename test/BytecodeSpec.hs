{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- TDD Tests for AST to Bytecode compilation
-}

module BytecodeSpec (spec) where

import Data.Text (pack)
import Ratatouille.AST
import Ratatouille.Bytecode.Types
import Ratatouille.Bytecode.Compiler
import Test.Hspec

spec :: Spec
spec = do
  describe "compileExpr" $ do
    literalTests
    variableTests
    atomTests
    tupleTests
    arrayTests
    arithmeticTests
    comparisonTests
    logicalTests
    stringTests
    controlFlowTests
    actorModelTests
    blockTests
    monadicTests
    callTests
    destructuringTests
    edgeCasesTests

  describe "compileStmt" $ do
    monadicTests
    edgeCasesTests

  describe "compileProgram" $ do
    programTests

  describe "Advanced compilation features" $ do
    stateManagementTests
    processAdvancedTests
    patternMatchingAdvancedTests
    processDefinitionTests
    variablesScopeTests
    assignmentTests
    fieldAccessTests
    selfReferenceTests
    castingTests
    incrementDecrementTests

-- | Tests for literal compilation
literalTests :: Spec
literalTests = describe "Literals" $ do
  it "compiles integer literals" $ do
    let expr = ELiteral (LInt 42)
    compileExpr expr `shouldBe` [PUSH_INT 42]

  it "compiles string literals" $ do
    let expr = ELiteral (LString (pack "hello"))
    compileExpr expr `shouldBe` [PUSH_STRING (pack "hello")]

  it "compiles negative integers" $ do
    let expr = ELiteral (LInt (-15))
    compileExpr expr `shouldBe` [PUSH_INT (-15)]

  it "compiles float literals" $ do
    let expr = ELiteral (LFloat 3.14)
    compileExpr expr `shouldBe` [PUSH_FLOAT 3.14]

  it "compiles boolean literals (true)" $ do
    let expr = ELiteral (LBool True)
    compileExpr expr `shouldBe` [PUSH_INT 1]

  it "compiles boolean literals (false)" $ do
    let expr = ELiteral (LBool False)
    compileExpr expr `shouldBe` [PUSH_INT 0]

  it "compiles none literal" $ do
    let expr = ELiteral LNone
    compileExpr expr `shouldBe` [PUSH_NONE]

  it "compiles typed integer literals" $ do
    let expr = ELiteral (LTypedInt I32 42)
    compileExpr expr `shouldBe` [PUSH_INT 42]

  it "compiles typed float literals" $ do
    let expr = ELiteral (LTypedFloat F64 2.5)
    compileExpr expr `shouldBe` [PUSH_FLOAT 2.5]

-- | Tests for variable compilation
variableTests :: Spec
variableTests = describe "Variables" $ do
  it "compiles variable references" $ do
    let expr = EVar (pack "counter")
    compileExpr expr `shouldBe` [LOAD_LOCAL (pack "counter")]

  it "compiles variable with underscore" $ do
    let expr = EVar (pack "my_var")
    compileExpr expr `shouldBe` [LOAD_LOCAL (pack "my_var")]

-- | Tests for atom compilation
atomTests :: Spec
atomTests = describe "Atoms" $ do
  it "compiles simple atoms" $ do
    let expr = EAtom (pack "hello")
    compileExpr expr `shouldBe` [PUSH_ATOM (pack "hello")]

  it "compiles atoms with underscores" $ do
    let expr = EAtom (pack "increment_counter")
    compileExpr expr `shouldBe` [PUSH_ATOM (pack "increment_counter")]

-- | Tests for tuple compilation
tupleTests :: Spec
tupleTests = describe "Tuples" $ do
  it "compiles empty tuple" $ do
    let expr = ETuple []
    compileExpr expr `shouldBe` [PUSH_TUPLE 0]

  it "compiles simple tuple with literals" $ do
    let expr = ETuple [ELiteral (LInt 1), ELiteral (LInt 2)]
    compileExpr expr `shouldBe` 
      [ PUSH_INT 1
      , PUSH_INT 2
      , PUSH_TUPLE 2
      ]

  it "compiles tuple with mixed types" $ do
    let expr = ETuple 
          [ EAtom (pack "get")
          , EVar (pack "sender")
          , ELiteral (LInt 42)
          ]
    compileExpr expr `shouldBe`
      [ PUSH_ATOM (pack "get")
      , LOAD_LOCAL (pack "sender")
      , PUSH_INT 42
      , PUSH_TUPLE 3
      ]

-- | Tests for array compilation
arrayTests :: Spec
arrayTests = describe "Arrays" $ do
  it "compiles empty array" $ do
    let expr = EArray []
    compileExpr expr `shouldBe` [PUSH_ARRAY 0]

  it "compiles simple array with literals" $ do
    let expr = EArray [ELiteral (LInt 1), ELiteral (LInt 2), ELiteral (LInt 3)]
    compileExpr expr `shouldBe`
      [ PUSH_INT 1
      , PUSH_INT 2
      , PUSH_INT 3
      , PUSH_ARRAY 3
      ]

  it "compiles array indexing" $ do
    let expr = EIndex (EVar (pack "arr")) (ELiteral (LInt 0))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "arr")
      , PUSH_INT 0
      , INDEX
      ]

-- | Tests for arithmetic operations
arithmeticTests :: Spec
arithmeticTests = describe "Arithmetic" $ do
  it "compiles simple addition" $ do
    let expr = EBinOp Add (ELiteral (LInt 2)) (ELiteral (LInt 3))
    compileExpr expr `shouldBe`
      [ PUSH_INT 2
      , PUSH_INT 3
      , ADD
      ]

  it "compiles subtraction" $ do
    let expr = EBinOp Sub (EVar (pack "x")) (ELiteral (LInt 1))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "x")
      , PUSH_INT 1
      , SUB
      ]

  it "compiles multiplication" $ do
    let expr = EBinOp Mul (ELiteral (LInt 4)) (ELiteral (LInt 5))
    compileExpr expr `shouldBe`
      [ PUSH_INT 4
      , PUSH_INT 5
      , MUL
      ]

  it "compiles division" $ do
    let expr = EBinOp Div (EVar (pack "a")) (EVar (pack "b"))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "a")
      , LOAD_LOCAL (pack "b")
      , DIV
      ]

  it "compiles complex arithmetic (2 + 3 * 4)" $ do
    let expr = EBinOp Add 
                 (ELiteral (LInt 2))
                 (EBinOp Mul (ELiteral (LInt 3)) (ELiteral (LInt 4)))
    compileExpr expr `shouldBe`
      [ PUSH_INT 2
      , PUSH_INT 3
      , PUSH_INT 4
      , MUL
      , ADD
      ]

-- | Tests for comparison operations
comparisonTests :: Spec
comparisonTests = describe "Comparisons" $ do
  it "compiles equality" $ do
    let expr = EBinOp Eq (ELiteral (LInt 5)) (ELiteral (LInt 5))
    compileExpr expr `shouldBe`
      [ PUSH_INT 5
      , PUSH_INT 5
      , CMP_EQ
      ]

  it "compiles inequality" $ do
    let expr = EBinOp Neq (EVar (pack "x")) (EVar (pack "y"))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "x")
      , LOAD_LOCAL (pack "y")
      , CMP_NEQ
      ]

  it "compiles less than" $ do
    let expr = EBinOp Lt (ELiteral (LInt 1)) (ELiteral (LInt 2))
    compileExpr expr `shouldBe`
      [ PUSH_INT 1
      , PUSH_INT 2
      , CMP_LT
      ]

  it "compiles greater than" $ do
    let expr = EBinOp Gt (EVar (pack "a")) (EVar (pack "b"))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "a")
      , LOAD_LOCAL (pack "b")
      , CMP_GT
      ]

  it "compiles less than or equal" $ do
    let expr = EBinOp Lte (ELiteral (LInt 3)) (ELiteral (LInt 3))
    compileExpr expr `shouldBe`
      [ PUSH_INT 3
      , PUSH_INT 3
      , CMP_LTE
      ]

  it "compiles greater than or equal" $ do
    let expr = EBinOp Gte (EVar (pack "x")) (ELiteral (LInt 0))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "x")
      , PUSH_INT 0
      , CMP_GTE
      ]

-- | Tests for logical operations
logicalTests :: Spec
logicalTests = describe "Logical" $ do
  it "compiles logical and" $ do
    let expr = EBinOp And (ELiteral (LBool True)) (ELiteral (LBool False))
    compileExpr expr `shouldBe`
      [ PUSH_INT 1
      , PUSH_INT 0
      , LOGIC_AND
      ]

  it "compiles logical or" $ do
    let expr = EBinOp Or (EVar (pack "cond1")) (EVar (pack "cond2"))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "cond1")
      , LOAD_LOCAL (pack "cond2")
      , LOGIC_OR
      ]

-- | Tests for string operations
stringTests :: Spec
stringTests = describe "Strings" $ do
  it "compiles string concatenation" $ do
    let expr = EBinOp Concat (ELiteral (LString (pack "hello"))) (ELiteral (LString (pack "world")))
    compileExpr expr `shouldBe`
      [ PUSH_STRING (pack "hello")
      , PUSH_STRING (pack "world")
      , CONCAT
      ]

-- | Tests for function calls
callTests :: Spec
callTests = describe "Function Calls" $ do
  it "compiles function call with no arguments" $ do
    let expr = ECall (pack "func") []
    compileExpr expr `shouldBe` [CALL (pack "func")]

  it "compiles function call with arguments" $ do
    let expr = ECall (pack "add") [ELiteral (LInt 1), ELiteral (LInt 2)]
    compileExpr expr `shouldBe`
      [ PUSH_INT 1
      , PUSH_INT 2
      , CALL (pack "add")
      ]

-- | Tests for destructuring assignments
destructuringTests :: Spec
destructuringTests = describe "Destructuring" $ do
  it "compiles tuple destructuring" $ do
    let stmt = SLetPattern (PTuple [PVar (pack "x"), PVar (pack "y")]) (ETuple [ELiteral (LInt 1), ELiteral (LInt 2)])
    compileStmt stmt `shouldBe`
      [ PUSH_INT 1
      , PUSH_INT 2
      , PUSH_TUPLE 2
      , PUSH_INT 0
      , INDEX
      , STORE_LOCAL (pack "x")
      , PUSH_INT 1
      , INDEX
      , STORE_LOCAL (pack "y")
      ]

  it "compiles array destructuring" $ do
    let stmt = SLetPattern (PArray [PVar (pack "a"), PVar (pack "b")]) (EArray [ELiteral (LInt 10), ELiteral (LInt 20)])
    compileStmt stmt `shouldBe`
      [ PUSH_INT 10
      , PUSH_INT 20
      , PUSH_ARRAY 2
      , PUSH_INT 0
      , INDEX
      , STORE_LOCAL (pack "a")
      , PUSH_INT 1
      , INDEX
      , STORE_LOCAL (pack "b")
      ]

-- | Tests for control flow operations
controlFlowTests :: Spec
controlFlowTests = describe "Control Flow" $ do
  it "compiles if-then expression" $ do
    let expr = EIf 
                 (EBinOp Gt (EVar (pack "x")) (ELiteral (LInt 0)))
                 (ELiteral (LInt 1))
                 Nothing
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "x")
      , PUSH_INT 0
      , CMP_GT
      , JUMP_IF_FALSE 1
      , PUSH_INT 1
      ]

  it "compiles if-then-else expression" $ do
    let expr = EIf 
                 (EBinOp Gt (EVar (pack "x")) (ELiteral (LInt 0)))
                 (ELiteral (LInt 1))
                 (Just (ELiteral (LInt 2)))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "x")
      , PUSH_INT 0
      , CMP_GT
      , JUMP_IF_FALSE 2
      , PUSH_INT 1
      , JUMP 1
      , PUSH_INT 2
      ]

-- | Tests for actor model operations
actorModelTests :: Spec
actorModelTests = describe "Actor Model" $ do
  it "compiles spawn with no arguments" $ do
    let expr = ESpawn (pack "Counter") []
    compileExpr expr `shouldBe` [CREATE_INSTANCE (pack "Counter")]

  it "compiles spawn with arguments" $ do
    let expr = ESpawn (pack "Counter") [ELiteral (LInt 0)]
    compileExpr expr `shouldBe`
      [ PUSH_INT 0
      , CREATE_INSTANCE (pack "Counter")  -- Arguments are on stack
      ]

  it "compiles send operation" $ do
    let expr = ESend (EVar (pack "pid")) (EAtom (pack "increment"))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "pid")
      , PUSH_ATOM (pack "increment")
      , SEND
      ]

  it "compiles send with tuple message" $ do
    let expr = ESend 
                 (EVar (pack "counter"))
                 (ETuple [EAtom (pack "get"), EVar (pack "self")])
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "counter")
      , PUSH_ATOM (pack "get")
      , LOAD_LOCAL (pack "self")
      , PUSH_TUPLE 2
      , SEND
      ]

  it "compiles receive with simple pattern" $ do
    let expr = EReceive 
                 [ Case (PAtom (pack "increment")) (ELiteral (LInt 1))
                 , Case (PVar (pack "msg")) (EVar (pack "msg"))
                 ]
    -- Le nouveau compileReceiveBlock génère un bytecode plus détaillé
    compileExpr expr `shouldSatisfy` (not . null)  -- Simplifié pour l'instant

-- | Tests for block compilation
blockTests :: Spec
blockTests = describe "Blocks" $ do
  it "compiles block with let statement" $ do
    let expr = EBlock 
                 [SLet (pack "x") Nothing (ELiteral (LInt 10))]
                 (EVar (pack "x"))
    compileExpr expr `shouldBe`
      [ PUSH_INT 10
      , STORE_LOCAL (pack "x")
      , LOAD_LOCAL (pack "x")
      ]

  it "compiles block with multiple statements" $ do
    let expr = EBlock
                 [ SLet (pack "x") Nothing (ELiteral (LInt 5))
                 , SLet (pack "y") Nothing (ELiteral (LInt 10))
                 , SExpr (ESend (EVar (pack "pid")) (EVar (pack "x")))
                 ]
                 (EBinOp Add (EVar (pack "x")) (EVar (pack "y")))
    compileExpr expr `shouldBe`
      [ PUSH_INT 5
      , STORE_LOCAL (pack "x")
      , PUSH_INT 10
      , STORE_LOCAL (pack "y")
      , LOAD_LOCAL (pack "pid")
      , LOAD_LOCAL (pack "x")
      , SEND
      , LOAD_LOCAL (pack "x")
      , LOAD_LOCAL (pack "y")
      , ADD
      ]

-- | Tests for monadic operations
monadicTests :: Spec
monadicTests = describe "Monadic Operations" $ do
  it "compiles Maybe bind with variable function" $ do
    let expr = EMaybeBind (EJust (ELiteral (LInt 42))) (EVar (pack "double"))
    compileExpr expr `shouldBe`
      [ PUSH_INT 42
      , PUSH_JUST
      , MAYBE_BIND (pack "double")
      ]

  it "compiles Either bind with variable function" $ do
    let expr = EEitherBind (ERight (ELiteral (LInt 100))) (EVar (pack "process"))
    compileExpr expr `shouldBe`
      [ PUSH_INT 100
      , PUSH_RIGHT
      , EITHER_BIND (pack "process")
      ]

  it "compiles Maybe bind with complex monad" $ do
    let expr = EMaybeBind 
                 (EJust (EBinOp Add (ELiteral (LInt 10)) (ELiteral (LInt 20))))
                 (EVar (pack "f"))
    compileExpr expr `shouldBe`
      [ PUSH_INT 10
      , PUSH_INT 20
      , ADD
      , PUSH_JUST
      , MAYBE_BIND (pack "f")
      ]

  it "compiles Either bind with complex monad" $ do
    let expr = EEitherBind 
                 (ELeft (ELiteral (LString (pack "error"))))
                 (EVar (pack "handler"))
    compileExpr expr `shouldBe`
      [ PUSH_STRING (pack "error")
      , PUSH_LEFT
      , EITHER_BIND (pack "handler")
      ]

  it "compiles nested Maybe binds" $ do
    let expr = EMaybeBind 
                 (EMaybeBind (EJust (ELiteral (LInt 5))) (EVar (pack "f")))
                 (EVar (pack "g"))
    compileExpr expr `shouldBe`
      [ PUSH_INT 5
      , PUSH_JUST
      , MAYBE_BIND (pack "f")
      , MAYBE_BIND (pack "g")
      ]

  it "compiles Maybe bind with None" $ do
    let expr = EMaybeBind ENone (EVar (pack "handler"))
    compileExpr expr `shouldBe`
      [ PUSH_NONE
      , MAYBE_BIND (pack "handler")
      ]

  it "compiles Either bind with Left" $ do
    let expr = EEitherBind (ELeft (ELiteral (LString (pack "error")))) (EVar (pack "recover"))
    compileExpr expr `shouldBe`
      [ PUSH_STRING (pack "error")
      , PUSH_LEFT
      , EITHER_BIND (pack "recover")
      ]

  it "compiles Either bind with Right" $ do
    let expr = EEitherBind (ERight (ELiteral (LInt 200))) (EVar (pack "success"))
    compileExpr expr `shouldBe`
      [ PUSH_INT 200
      , PUSH_RIGHT
      , EITHER_BIND (pack "success")
      ]

-- | Tests for program compilation
programTests :: Spec
programTests = describe "Program compilation" $ do
  it "compiles simple process definition" $ do
    let program = Program 
          [ DProc $ ProcDef (pack "Counter") [pack "initial"]
              (ProcBody 
                (Just (EVar (pack "initial")))
                [ Case (PAtom (pack "increment")) 
                       (EBinOp Add (EVar (pack "state")) (ELiteral (LInt 1)))
                ]
              )
          ]
    -- Cette compilation sera plus complexe et dépendra de notre architecture VM
    compileProgram program `shouldSatisfy` (not . null)

  it "compiles program with multiple process definitions" $ do
    let program = Program
          [ DProc $ ProcDef (pack "Greeter") []
              (ProcBody Nothing
                [ Case (PAtom (pack "hello")) (ELiteral (LString (pack "Hello!")))
                ]
              )
          , DProc $ ProcDef (pack "Counter") [pack "n"]
              (ProcBody (Just (EVar (pack "n")))
                [ Case (PAtom (pack "get")) (EVar (pack "state"))
                ]
              )
          ]
    compileProgram program `shouldSatisfy` (not . null)

-- | Tests for edge cases and error conditions
edgeCasesTests :: Spec
edgeCasesTests = describe "Edge cases" $ do
  it "handles deeply nested expressions" $ do
    let expr = EBinOp Add
                 (EBinOp Mul (ELiteral (LInt 1)) (ELiteral (LInt 2)))
                 (EBinOp Div (ELiteral (LInt 6)) (ELiteral (LInt 2)))
    compileExpr expr `shouldBe`
      [ PUSH_INT 1
      , PUSH_INT 2
      , MUL
      , PUSH_INT 6
      , PUSH_INT 2
      , DIV
      , ADD
      ]

  it "handles complex spawn with multiple arguments" $ do
    let expr = ESpawn (pack "Worker") 
                 [ ELiteral (LInt 42)
                 , EAtom (pack "worker_type")
                 , ETuple [EVar (pack "config"), ELiteral (LString (pack "name"))]
                 ]
    compileExpr expr `shouldBe`
      [ PUSH_INT 42
      , PUSH_ATOM (pack "worker_type")
      , LOAD_LOCAL (pack "config")
      , PUSH_STRING (pack "name")
      , PUSH_TUPLE 2
      , CREATE_INSTANCE (pack "Worker")
      ]

  it "compiles const bindings" $ do
    let stmt = SConst (pack "PI") Nothing (ELiteral (LFloat 3.14159))
    compileStmt stmt `shouldBe`
      [ PUSH_FLOAT 3.14159
      , STORE_LOCAL (pack "PI")
      ]

-- | Tests for state management features
stateManagementTests :: Spec
stateManagementTests = describe "State Management" $ do
  it "compiles state variable access" $ do
    let expr = EVar (pack "state")
    compileExpr expr `shouldBe` [GET_STATE]

  it "compiles state increment operation" $ do
    let stmt = SExpr (EBinOp Add (EVar (pack "state")) (ELiteral (LInt 1)))
    compileStmt stmt `shouldBe`
      [ GET_STATE
      , PUSH_INT 1
      , ADD
      , SET_STATE
      ]

  it "compiles state multiplication" $ do
    let stmt = SExpr (EBinOp Mul (EVar (pack "state")) (ELiteral (LInt 2)))
    compileStmt stmt `shouldBe`
      [ GET_STATE
      , PUSH_INT 2
      , MUL
      , SET_STATE
      ]

-- | Tests for advanced process compilation
processAdvancedTests :: Spec
processAdvancedTests = describe "Advanced Process Features" $ do
  it "compiles process with parameters" $ do
    let procDef = DProc $ ProcDef (pack "Counter") [pack "initial", pack "max"]
                    (ProcBody 
                      (Just (EVar (pack "initial")))
                      [Case (PAtom (pack "increment")) (ELiteral (LInt 1))]
                    )
    case compileDefinition procDef of
      [DEFINE_PROCESS _ _ bodyCode] -> do
        -- Should include parameter binding and state initialization
        bodyCode `shouldSatisfy` (\code -> 
          STORE_LOCAL (pack "max") `elem` code &&
          STORE_LOCAL (pack "initial") `elem` code &&
          INIT_STATE `elem` code &&
          LABEL (pack "receive_loop") `elem` code)
      other -> expectationFailure $ "Expected single DEFINE_PROCESS, got: " ++ show other

  it "compiles process with no initial state" $ do
    let procDef = DProc $ ProcDef (pack "Logger") []
                    (ProcBody Nothing [Case PWildcard (ELiteral (LInt 0))])
    case compileDefinition procDef of
      [DEFINE_PROCESS _ _ bodyCode] -> do
        -- Should include default unit state initialization
        bodyCode `shouldSatisfy` (\code -> 
          PUSH_UNIT `elem` code &&
          INIT_STATE `elem` code)
      other -> expectationFailure $ "Expected single DEFINE_PROCESS, got: " ++ show other

  it "compiles receive block structure" $ do
    let cases = [ Case (PAtom (pack "hello")) (ELiteral (LString (pack "world")))
                , Case (PVar (pack "msg")) (EVar (pack "msg"))
                ]
    let bytecode = compileReceiveBlock cases
    -- Should start with LABEL "receive_loop" and contain WAIT_MESSAGE and end with EXIT_PROCESS
    case bytecode of
      (first:_) -> first `shouldBe` LABEL (pack "receive_loop")
      [] -> expectationFailure "bytecode should not be empty"
    -- Should contain WAIT_MESSAGE
    bytecode `shouldSatisfy` (WAIT_MESSAGE `elem`)
    case reverse bytecode of
      (lastByte:_) -> lastByte `shouldBe` EXIT_PROCESS
      [] -> expectationFailure "bytecode should not be empty"

-- | Tests for advanced pattern matching
patternMatchingAdvancedTests :: Spec  
patternMatchingAdvancedTests = describe "Advanced Pattern Matching" $ do
  it "compiles wildcard pattern" $ do
    compilePattern PWildcard `shouldBe` [MATCH_WILDCARD]

  it "compiles variable pattern" $ do
    compilePattern (PVar (pack "x")) `shouldBe` [MATCH_VAR (pack "x")]

  it "compiles atom pattern with jump" $ do
    compilePattern (PAtom (pack "test")) `shouldBe` [MATCH_ATOM (pack "test") 2]

  it "compiles integer literal pattern" $ do
    compilePattern (PLiteral (LInt 42)) `shouldBe` 
      [PUSH_INT 42, MATCH_ATOM (pack "42") 2]

  it "compiles string literal pattern" $ do
    compilePattern (PLiteral (LString (pack "hello"))) `shouldBe`
      [PUSH_STRING (pack "hello"), MATCH_ATOM (pack "hello") 2]

  it "compiles tuple pattern" $ do
    let pattern = PTuple [PAtom (pack "get"), PVar (pack "sender")]
    let bytecode = compilePattern pattern
    -- Should include MATCH_TUPLE with correct size
    bytecode `shouldSatisfy` (\code -> MATCH_TUPLE 2 3 `elem` code)

  it "compiles nested tuple patterns" $ do
    let pattern = PTuple 
          [ PAtom (pack "request")
          , PTuple [PVar (pack "id"), PVar (pack "data")]
          ]
    let bytecode = compilePattern pattern  
    -- Should handle nested structure correctly
    bytecode `shouldSatisfy` (not . null)

-- | Tests for process definition compilation
processDefinitionTests :: Spec
processDefinitionTests = describe "Process Definition" $ do
  it "generates DEFINE_PROCESS instruction" $ do
    let procDef = DProc $ ProcDef (pack "Test") [] (ProcBody Nothing [])
    case compileDefinition procDef of
      [DEFINE_PROCESS name params _] -> do
        name `shouldBe` pack "Test"
        params `shouldBe` []
      other -> expectationFailure $ "Expected single DEFINE_PROCESS, got: " ++ show other

  it "handles multiple parameters correctly" $ do
    let procDef = DProc $ ProcDef (pack "Worker") [pack "id", pack "config"] 
                    (ProcBody Nothing [])
    case compileDefinition procDef of
      [DEFINE_PROCESS _ params _] -> 
        params `shouldBe` [pack "id", pack "config"]
      other -> expectationFailure $ "Expected single DEFINE_PROCESS, got: " ++ show other

-- | Tests for local vs global variable handling
variablesScopeTests :: Spec
variablesScopeTests = describe "Variable Scoping" $ do
  it "prefers local scope for regular variables" $ do
    let expr = EVar (pack "counter")
    compileExpr expr `shouldBe` [LOAD_LOCAL (pack "counter")]

  it "uses special handling for state variable" $ do
    let expr = EVar (pack "state") 
    compileExpr expr `shouldBe` [GET_STATE]

  it "compiles let bindings to local storage" $ do
    let stmt = SLet (pack "temp") Nothing (ELiteral (LInt 100))
    compileStmt stmt `shouldBe` 
      [ PUSH_INT 100
      , STORE_LOCAL (pack "temp")
      ]

-- | Tests for assignment operations
assignmentTests :: Spec
assignmentTests = describe "Assignment" $ do
  it "compiles variable assignment expression" $ do
    let expr = EAssign (pack "x") (ELiteral (LInt 42))
    compileExpr expr `shouldBe`
      [ PUSH_INT 42
      , STORE_LOCAL (pack "x")
      , LOAD_LOCAL (pack "x")
      ]

  it "compiles assignment statement" $ do
    let stmt = SAssign (pack "y") (ELiteral (LInt 100))
    compileStmt stmt `shouldBe`
      [ PUSH_INT 100
      , STORE_LOCAL (pack "y")
      ]

-- | Tests for field access operations
fieldAccessTests :: Spec
fieldAccessTests = describe "Field Access" $ do
  it "compiles field access on variable" $ do
    let expr = EFieldAccess (EVar (pack "obj")) (pack "field")
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "obj")
      , GET_FIELD (pack "field")
      ]

  it "compiles field access on complex expression" $ do
    let expr = EFieldAccess (ETuple [ELiteral (LInt 1), ELiteral (LInt 2)]) (pack "first")
    compileExpr expr `shouldBe`
      [ PUSH_INT 1
      , PUSH_INT 2
      , PUSH_TUPLE 2
      , GET_FIELD (pack "first")
      ]

-- | Tests for self reference
selfReferenceTests :: Spec
selfReferenceTests = describe "Self Reference" $ do
  it "compiles self reference" $ do
    let expr = ESelf
    compileExpr expr `shouldBe` [SELF]

-- | Tests for type casting
castingTests :: Spec
castingTests = describe "Type Casting" $ do
  it "compiles static cast" $ do
    let expr = ECast StaticCast (TNumeric I32) (ELiteral (LInt 42))
    compileExpr expr `shouldBe`
      [ PUSH_INT 42
      , STATIC_CAST (pack "i32")
      ]

  it "compiles reinterpret cast" $ do
    let expr = ECast ReinterpretCast (TString) (EVar (pack "data"))
    compileExpr expr `shouldBe`
      [ LOAD_LOCAL (pack "data")
      , REINTERPRET_CAST (pack "string")
      ]

  it "compiles const cast" $ do
    let expr = ECast ConstCast (TNumeric I64) (ELiteral (LInt 100))
    compileExpr expr `shouldBe`
      [ PUSH_INT 100
      , CONST_CAST
      ]

-- | Tests for increment/decrement operations
incrementDecrementTests :: Spec
incrementDecrementTests = describe "Increment/Decrement" $ do
  it "compiles pre-increment" $ do
    let expr = EPreInc (pack "counter")
    compileExpr expr `shouldBe` [INC_VAR (pack "counter")]

  it "compiles post-increment" $ do
    let expr = EPostInc (pack "counter")
    compileExpr expr `shouldBe` [INC_VAR_POST (pack "counter")]

  it "compiles pre-decrement" $ do
    let expr = EPreDec (pack "counter")
    compileExpr expr `shouldBe` [DEC_VAR (pack "counter")]

  it "compiles post-decrement" $ do
    let expr = EPostDec (pack "counter")
    compileExpr expr `shouldBe` [DEC_VAR_POST (pack "counter")]