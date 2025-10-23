{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- ParserSpec
-}

module ParserSpec (spec) where

import Data.Text (Text, pack)
import Ratatouille.AST
  ( Definition (..),
    Expr
      ( EAssign,
        EAtom,
        EBinOp,
        EBlock,
        ECall,
        ELiteral,
        ESend,
        ESpawn,
        ETuple,
        EVar
      ),
    Literal (LInt, LString),
    Op (Add, Div, Mul, Sub),
    Pattern (PAtom, PTuple, PVar),
    ProcBody (ProcBody),
    ProcDefinition (ProcDef),
    Program (..),
    ReceiveCase (Case),
    Stmt (SExpr, SLet),
  )
import Ratatouille.Parser.Common (sc)
import Ratatouille.Parser.ExprStmt (pStatement)
import Ratatouille.Parser.Proc (pDefinition, pProgram)
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
  )
import Text.Megaparsec (eof, errorBundlePretty, parse)

-- | Helper to test a successful parsing of a Statement.
-- Uses 'pStatement' for parsing.
shouldParseStmtAs :: Text -> Stmt -> Expectation
shouldParseStmtAs input expected =
  case parse (sc *> pStatement <* eof) "" input of
    Left err -> expectationFailure $ "Parsing statement failed unexpectedly:\n" ++ errorBundlePretty err
    Right result -> result `shouldBe` expected

-- | Helper to test a successful parsing of a full Program.
-- Uses 'pProgram' for parsing, which handles leading/trailing whitespace and EOF.
shouldParseAsProgram :: Text -> Program -> Expectation
shouldParseAsProgram input expected =
  case parse pProgram "" input of
    Left err -> expectationFailure $ "Parsing program failed unexpectedly:\n" ++ errorBundlePretty err
    Right result -> result `shouldBe` expected

-- | Helper to test a parsing that is expected to fail (on 'pProgram').
shouldFail :: Text -> Expectation
shouldFail input =
  case parse pProgram "" input of
    Left _ -> pure () -- Success: parsing failed as expected.
    Right res -> expectationFailure $ "Parsing was supposed to fail, but succeeded with: " ++ show res

-- | Helper to test a successful parsing of a top-level Definition.
-- Uses 'pDefinition' for parsing.
shouldParseDefAs :: Text -> Definition -> Expectation
shouldParseDefAs input expected =
  case parse (sc *> pDefinition <* eof) "" input of
    Left err -> expectationFailure $ "Parsing definition failed unexpectedly:\n" ++ errorBundlePretty err
    Right result -> result `shouldBe` expected

-- | Main test suite specification.
spec :: Spec
spec = describe "Ratatouille Parser" $ do
  describe "Statements (pStatement)" $ do
    it "parses a positive integer as an SExpr statement" $
      shouldParseStmtAs (pack "123") (SExpr (ELiteral (LInt 123)))

    it "parses a negative integer as an SExpr statement" $
      shouldParseStmtAs (pack "-45") (SExpr (ELiteral (LInt (-45))))

    it "parses a string literal as an SExpr statement" $
      shouldParseStmtAs (pack "\"Hello Ratatouille!\"") (SExpr (ELiteral (LString (pack "Hello Ratatouille!"))))

    it "parses an atom as an SExpr statement" $
      shouldParseStmtAs (pack ":myAtom") (SExpr (EAtom (pack "myAtom")))

    it "handles whitespace around literals in SExpr" $
      shouldParseStmtAs (pack "  123  ") (SExpr (ELiteral (LInt 123)))

    it "handles whitespace around atoms in SExpr" $
      shouldParseStmtAs (pack " :another_atom ") (SExpr (EAtom (pack "another_atom")))

    it "parses a variable identifier as an SExpr statement" $
      shouldParseStmtAs (pack "my_variable") (SExpr (EVar (pack "my_variable")))

    it "fails to parse an identifier starting with a number" $
      shouldFail (pack "1variable")

    it "parses an empty tuple as an SExpr statement" $
      shouldParseStmtAs (pack "{}") (SExpr (ETuple []))

    it "parses a single-element tuple as an SExpr statement" $
      shouldParseStmtAs (pack "{ 123 }") (SExpr (ETuple [ELiteral (LInt 123)]))

    it "parses a multi-element tuple as an SExpr statement" $
      shouldParseStmtAs
        (pack "{ :deposit, 50, my_account }")
        (SExpr (ETuple [EAtom (pack "deposit"), ELiteral (LInt 50), EVar (pack "my_account")]))

    it "parses a tuple with atom containing underscore" $
      shouldParseStmtAs
        (pack "{ :balance_is, state }")
        (SExpr (ETuple [EAtom (pack "balance_is"), EVar (pack "state")]))

    it "parses a tuple with trailing comma as an SExpr statement" $
      shouldParseStmtAs (pack "{ 1, 2, }") (SExpr (ETuple [ELiteral (LInt 1), ELiteral (LInt 2)]))

    it "parses a function call with arguments as an SExpr statement" $
      shouldParseStmtAs
        (pack "{ :my_function, :arg1, 123, \"hello\" }")
        ( SExpr
            ( ETuple
                [ EAtom (pack "my_function"),
                  EAtom (pack "arg1"),
                  ELiteral (LInt 123),
                  ELiteral (LString (pack "hello"))
                ]
            )
        )

    it "parses a nested function call as an SExpr statement" $
      shouldParseStmtAs
        (pack "{ :outer_func, { :inner_func, 10 }, 20 }")
        ( SExpr
            ( ETuple
                [ EAtom (pack "outer_func"),
                  ETuple [EAtom (pack "inner_func"), ELiteral (LInt 10)],
                  ELiteral (LInt 20)
                ]
            )
        )

    it "parses a function call with variable as function name as an SExpr statement" $
      shouldParseStmtAs
        (pack "{ my_func_var, 1, 2 }")
        ( SExpr
            ( ETuple
                [ EVar (pack "my_func_var"),
                  ELiteral (LInt 1),
                  ELiteral (LInt 2)
                ]
            )
        )

    it "parses simple addition as an SExpr statement" $
      shouldParseStmtAs (pack "1 + 2") (SExpr (EBinOp Add (ELiteral (LInt 1)) (ELiteral (LInt 2))))

    it "parses simple multiplication as an SExpr statement" $
      shouldParseStmtAs (pack "3 * 4") (SExpr (EBinOp Mul (ELiteral (LInt 3)) (ELiteral (LInt 4))))

    it "respects operator precedence (multiplication before addition) as an SExpr statement" $
      shouldParseStmtAs
        (pack "1 + 2 * 3")
        ( SExpr
            ( EBinOp
                Add
                (ELiteral (LInt 1))
                (EBinOp Mul (ELiteral (LInt 2)) (ELiteral (LInt 3)))
            )
        )

    it "handles left associativity (subtraction) as an SExpr statement" $
      shouldParseStmtAs
        (pack "10 - 5 - 2")
        ( SExpr
            ( EBinOp
                Sub
                (EBinOp Sub (ELiteral (LInt 10)) (ELiteral (LInt 5)))
                (ELiteral (LInt 2))
            )
        )

    it "handles parentheses for custom precedence as an SExpr statement" $
      shouldParseStmtAs
        (pack "(1 + 2) * 3")
        ( SExpr
            ( EBinOp
                Mul
                (EBinOp Add (ELiteral (LInt 1)) (ELiteral (LInt 2)))
                (ELiteral (LInt 3))
            )
        )

    it "parses complex expression with mixed operators as an SExpr statement" $
      shouldParseStmtAs
        (pack "10 / 2 + 5 * 3 - 1")
        ( SExpr
            ( EBinOp
                Sub
                ( EBinOp
                    Add
                    (EBinOp Div (ELiteral (LInt 10)) (ELiteral (LInt 2)))
                    (EBinOp Mul (ELiteral (LInt 5)) (ELiteral (LInt 3)))
                )
                (ELiteral (LInt 1))
            )
        )

    it "parses a simple let statement" $
      shouldParseStmtAs (pack "let x = 42") (SLet (pack "x") (ELiteral (LInt 42)))

    it "parses a let statement with a complex expression" $
      shouldParseStmtAs
        (pack "let result = (10 + 5) * 2")
        ( SLet
            (pack "result")
            ( EBinOp
                Mul
                (EBinOp Add (ELiteral (LInt 10)) (ELiteral (LInt 5)))
                (ELiteral (LInt 2))
            )
        )

    it "fails to parse a let statement without an identifier" $
      shouldFail (pack "let = 42")

    it "parses a binary operation as a statement" $
      shouldParseStmtAs
        (pack "10 + 20")
        (SExpr (EBinOp Add (ELiteral (LInt 10)) (ELiteral (LInt 20))))

    it "parses a message send as a statement" $
      shouldParseStmtAs
        (pack "my_pid <- { :msg, 42 }")
        ( SExpr
            ( ESend
                (EVar (pack "my_pid"))
                (ETuple [EAtom (pack "msg"), ELiteral (LInt 42)])
            )
        )

    it "parses a message send with atom containing underscore" $
      shouldParseStmtAs
        (pack "sender <- { :balance_is, state }")
        ( SExpr
            ( ESend
                (EVar (pack "sender"))
                (ETuple [EAtom (pack "balance_is"), EVar (pack "state")])
            )
        )

    it "parses a block with a single statement encapsulated in SExpr" $
      shouldParseStmtAs
        (pack "{ let x = 10 }")
        (SExpr (EBlock [SLet (pack "x") (ELiteral (LInt 10))] (ELiteral (LInt 0))))

    it "parses a block with multiple statements and a final expression encapsulated in SExpr" $
      shouldParseStmtAs
        (pack "{ let x = 10; let y = x + 5; y * 2 }")
        ( SExpr
            ( EBlock
                [ SLet (pack "x") (ELiteral (LInt 10)),
                  SLet (pack "y") (EBinOp Add (EVar (pack "x")) (ELiteral (LInt 5)))
                ]
                (EBinOp Mul (EVar (pack "y")) (ELiteral (LInt 2)))
            )
        )

    it "parses a block where the last statement is also the final expression encapsulated in SExpr" $
      shouldParseStmtAs
        (pack "{ let x = 10; x }")
        (SExpr (EBlock [SLet (pack "x") (ELiteral (LInt 10))] (EVar (pack "x"))))

    it "parses a block with an assignment as the final expression" $
      shouldParseStmtAs
        (pack "{ let x = 10; x = 20 }")
        (SExpr (EBlock [SLet (pack "x") (ELiteral (LInt 10))] (EAssign (pack "x") (ELiteral (LInt 20)))))

  describe "Program (Full sequence of statements)" $ do
    it "parses an empty program" $
      shouldParseAsProgram (pack "") (Program [])

    it "parses a program with a single statement" $
      shouldParseAsProgram
        (pack "let x = 10;")
        (Program [DStmt (SLet (pack "x") (ELiteral (LInt 10)))])

    it "parses a program with multiple statements" $
      shouldParseAsProgram
        (pack "let a = 1; let b = a + 2; b * 3")
        ( Program
            [ DStmt (SLet (pack "a") (ELiteral (LInt 1))),
              DStmt (SLet (pack "b") (EBinOp Add (EVar (pack "a")) (ELiteral (LInt 2)))),
              DStmt (SExpr (EBinOp Mul (EVar (pack "b")) (ELiteral (LInt 3))))
            ]
        )

  describe "Definitions (pDefinition)" $ do
    it "parses a simple proc definition with no params and empty body" $
      let expectedProc = ProcDef (pack "MyProc") [] (ProcBody Nothing [])
       in shouldParseDefAs (pack "proc MyProc() {}") (DProc expectedProc)

    it "parses a proc definition with parameters" $
      let expectedProc = ProcDef (pack "Counter") [pack "initial_value"] (ProcBody Nothing [])
       in shouldParseDefAs (pack "proc Counter(initial_value) {}") (DProc expectedProc)

    it "parses a proc definition with a state expression" $
      let stateExpr = ELiteral (LInt 100)
          expectedProc = ProcDef (pack "Wallet") [] (ProcBody (Just stateExpr) [])
       in shouldParseDefAs (pack "proc Wallet() { state: 100 }") (DProc expectedProc)

    it "parses a proc definition with a receive block" $
      let receiveCase = Case (PVar (pack "msg")) (EVar (pack "msg"))
          expectedProc = ProcDef (pack "Logger") [] (ProcBody Nothing [receiveCase])
       in shouldParseDefAs (pack "proc Logger() { receive { | msg -> msg } }") (DProc expectedProc)

    it "parses a full proc definition" $
      let stateExpr = ETuple []
          receiveCase =
            Case
              (PTuple [PAtom (pack "deposit"), PVar (pack "amount")])
              (EBinOp Add (EVar (pack "state")) (EVar (pack "amount")))
          expectedProc = ProcDef (pack "Account") [pack "owner"] (ProcBody (Just stateExpr) [receiveCase])
       in shouldParseDefAs (pack "proc Account(owner) { state: {}, receive { | { :deposit, amount } -> state + amount } }") (DProc expectedProc)

  describe "Full Programs (Real-world scenarios)" $ do
    it "parses a complete 'Logger' program" $
      let programSource =
            pack $
              unlines
                [ "proc Logger() {",
                  "  receive {",
                  "    | msg -> print(msg)",
                  "  }",
                  "};",
                  "",
                  "let logger_pid = spawn Logger();",
                  "logger_pid <- \"Hello, World!\"",
                  ""
                ]
          expectedAST =
            Program
              [ DProc
                  ( ProcDef
                      (pack "Logger")
                      []
                      ( ProcBody
                          Nothing
                          [ Case (PVar (pack "msg")) (ECall (pack "print") [EVar (pack "msg")])
                          ]
                      )
                  ),
                DStmt (SLet (pack "logger_pid") (ESpawn (pack "Logger") [])),
                DStmt (SExpr (ESend (EVar (pack "logger_pid")) (ELiteral (LString (pack "Hello, World!")))))
              ]
       in shouldParseAsProgram programSource expectedAST

    it "parses a complete 'BankAccount' program" $
      let programSource =
            pack $
              unlines
                [ "proc BankAccount(initial_balance) {",
                  "  state: initial_balance,",
                  "  receive {",
                  "    | { :deposit, amount } -> state = state + amount",
                  "    | { :get_balance, sender } -> sender <- { :balance_is, state }",
                  "  }",
                  "};",
                  "",
                  "let my_account = spawn BankAccount(100);",
                  "my_account <- { :deposit, 50 }",
                  ""
                ]
          expectedAST =
            Program
              [ DProc
                  ( ProcDef
                      (pack "BankAccount")
                      [pack "initial_balance"]
                      ( ProcBody
                          (Just (EVar (pack "initial_balance")))
                          [ Case
                              (PTuple [PAtom (pack "deposit"), PVar (pack "amount")])
                              (EAssign (pack "state") (EBinOp Add (EVar (pack "state")) (EVar (pack "amount")))),
                            Case
                              (PTuple [PAtom (pack "get_balance"), PVar (pack "sender")])
                              (ESend (EVar (pack "sender")) (ETuple [EAtom (pack "balance_is"), EVar (pack "state")]))
                          ]
                      )
                  ),
                DStmt (SLet (pack "my_account") Nothing (ESpawn (pack "BankAccount") [ELiteral (LInt 100)])),
                DStmt (SExpr (ESend (EVar (pack "my_account")) (ETuple [EAtom (pack "deposit"), ELiteral (LInt 50)])))
              ]
       in shouldParseAsProgram programSource expectedAST
