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

  describe "compileProgram" $ do
    programTests

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
    compileExpr expr `shouldBe` [LOAD_VAR (pack "counter")]

  it "compiles variable with underscore" $ do
    let expr = EVar (pack "my_var")
    compileExpr expr `shouldBe` [LOAD_VAR (pack "my_var")]

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
      , LOAD_VAR (pack "sender")
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
      [ LOAD_VAR (pack "x")
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
      [ LOAD_VAR (pack "a")
      , LOAD_VAR (pack "b")
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
    compileExpr expr `shouldBe` [SPAWN (pack "Counter") []]

  it "compiles spawn with arguments" $ do
    let expr = ESpawn (pack "Counter") [ELiteral (LInt 0)]
    compileExpr expr `shouldBe`
      [ PUSH_INT 0
      , SPAWN (pack "Counter") []  -- Arguments are on stack
      ]

  it "compiles send operation" $ do
    let expr = ESend (EVar (pack "pid")) (EAtom (pack "increment"))
    compileExpr expr `shouldBe`
      [ LOAD_VAR (pack "pid")
      , PUSH_ATOM (pack "increment")
      , SEND
      ]

  it "compiles send with tuple message" $ do
    let expr = ESend 
                 (EVar (pack "counter"))
                 (ETuple [EAtom (pack "get"), EVar (pack "self")])
    compileExpr expr `shouldBe`
      [ LOAD_VAR (pack "counter")
      , PUSH_ATOM (pack "get")
      , LOAD_VAR (pack "self")
      , PUSH_TUPLE 2
      , SEND
      ]

  it "compiles receive with simple pattern" $ do
    let expr = EReceive 
                 [ Case (PAtom (pack "increment")) (ELiteral (LInt 1))
                 , Case (PVar (pack "msg")) (EVar (pack "msg"))
                 ]
    compileExpr expr `shouldBe`
      [ RECEIVE [ Case (PAtom (pack "increment")) (ELiteral (LInt 1))
                , Case (PVar (pack "msg")) (EVar (pack "msg"))
                ]
      ]

-- | Tests for block compilation
blockTests :: Spec
blockTests = describe "Blocks" $ do
  it "compiles block with let statement" $ do
    let expr = EBlock 
                 [SLet (pack "x") (ELiteral (LInt 10))]
                 (EVar (pack "x"))
    compileExpr expr `shouldBe`
      [ PUSH_INT 10
      , STORE_VAR (pack "x")
      , LOAD_VAR (pack "x")
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
      , STORE_VAR (pack "x")
      , PUSH_INT 10
      , STORE_VAR (pack "y")
      , LOAD_VAR (pack "pid")
      , LOAD_VAR (pack "x")
      , SEND
      , LOAD_VAR (pack "x")
      , LOAD_VAR (pack "y")
      , ADD
      ]

-- | Tests for program compilation
programTests :: Spec
programTests = describe "Program compilation" $ do
  it "compiles simple process definition" $ do
    let program = Program 
          [ ProcDef (pack "Counter") [pack "initial"]
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
          [ ProcDef (pack "Greeter") []
              (ProcBody Nothing
                [ Case (PAtom (pack "hello")) (ELiteral (LString (pack "Hello!")))
                ]
              )
          , ProcDef (pack "Counter") [pack "n"]
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
      , LOAD_VAR (pack "config")
      , PUSH_STRING (pack "name")
      , PUSH_TUPLE 2
      , SPAWN (pack "Worker") []
      ]