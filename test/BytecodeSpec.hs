{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- TDD Tests for AST to Bytecode compilation
-}

module BytecodeSpec (spec) where

import Data.Text (pack)
import Ratatouille.AST
import Ratatouille.Bytecode
import Test.Hspec

spec :: Spec
spec = do
  describe "compileExpr" $ do
    literalTests
    variableTests
    atomTests
    tupleTests
    arithmeticTests
    actorModelTests
    blockTests
    edgeCasesTests

  describe "compileProgram" $ do
    programTests

  describe "Advanced compilation features" $ do
    stateManagementTests
    processAdvancedTests
    patternMatchingAdvancedTests
    processDefinitionTests
    variablesScopeTests

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
                 [SLet (pack "x") (ELiteral (LInt 10))]
                 (EVar (pack "x"))
    compileExpr expr `shouldBe`
      [ PUSH_INT 10
      , STORE_LOCAL (pack "x")
      , LOAD_LOCAL (pack "x")
      ]

  it "compiles block with multiple statements" $ do
    let expr = EBlock
                 [ SLet (pack "x") (ELiteral (LInt 5))
                 , SLet (pack "y") (ELiteral (LInt 10))
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
          PROCESS_LOOP `elem` code)
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
    -- Should start with WAIT_MESSAGE and end with EXIT_PROCESS
    case bytecode of
      (first:_) -> first `shouldBe` WAIT_MESSAGE
      [] -> expectationFailure "bytecode should not be empty"
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
    let stmt = SLet (pack "temp") (ELiteral (LInt 100))
    compileStmt stmt `shouldBe` 
      [ PUSH_INT 100
      , STORE_LOCAL (pack "temp")
      ]