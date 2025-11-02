{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- AST module tests
-}

module ASTSpec (spec) where

import qualified Data.Text as T
import Ratatouille.AST
import Test.Hspec

spec :: Spec
spec = do
  describe "Program" $ do
    it "creates empty program" $ do
      let prog = Program []
      prog `shouldBe` Program []

    it "creates program with definitions" $ do
      let stmt = DStmt (SExpr (ELiteral (LInt 42)))
      let prog = Program [stmt]
      prog `shouldBe` Program [stmt]

    it "shows Program correctly" $ do
      let prog = Program []
      show prog `shouldContain` "Program"

    it "compares Programs for equality" $ do
      let prog1 = Program [DStmt (SExpr (ELiteral (LInt 1)))]
      let prog2 = Program [DStmt (SExpr (ELiteral (LInt 1)))]
      let prog3 = Program [DStmt (SExpr (ELiteral (LInt 2)))]
      prog1 `shouldBe` prog2
      prog1 `shouldNotBe` prog3

  describe "Definition" $ do
    it "creates DProc definition" $ do
      let pdef = ProcDef (T.pack "test") [] (ProcBody Nothing [])
      let def = DProc pdef
      def `shouldBe` DProc pdef

    it "creates DStmt definition" $ do
      let stmt = SLet (T.pack "x") Nothing (ELiteral (LInt 42))
      let def = DStmt stmt
      def `shouldBe` DStmt stmt

    it "shows DProc correctly" $ do
      let pdef = ProcDef (T.pack "test") [] (ProcBody Nothing [])
      show (DProc pdef) `shouldContain` "DProc"

    it "shows DStmt correctly" $ do
      let stmt = SExpr (ELiteral (LInt 5))
      show (DStmt stmt) `shouldContain` "DStmt"

    it "compares Definitions for equality" $ do
      let def1 = DStmt (SExpr (ELiteral (LInt 1)))
      let def2 = DStmt (SExpr (ELiteral (LInt 1)))
      let def3 = DStmt (SExpr (ELiteral (LInt 2)))
      def1 `shouldBe` def2
      def1 `shouldNotBe` def3

  describe "ProcDefinition" $ do
    it "creates procedure with no parameters" $ do
      let pdef = ProcDef (T.pack "myProc") [] (ProcBody Nothing [])
      procName pdef `shouldBe` T.pack "myProc"
      procParams pdef `shouldBe` []

    it "creates procedure with parameters" $ do
      let pdef = ProcDef (T.pack "add") [T.pack "a", T.pack "b"] (ProcBody Nothing [])
      procName pdef `shouldBe` T.pack "add"
      procParams pdef `shouldBe` [T.pack "a", T.pack "b"]

    it "shows ProcDef correctly" $ do
      let pdef = ProcDef (T.pack "test") [] (ProcBody Nothing [])
      show pdef `shouldContain` "ProcDef"
      show pdef `shouldContain` "test"

    it "compares ProcDefinitions for equality" $ do
      let pdef1 = ProcDef (T.pack "p") [] (ProcBody Nothing [])
      let pdef2 = ProcDef (T.pack "p") [] (ProcBody Nothing [])
      let pdef3 = ProcDef (T.pack "q") [] (ProcBody Nothing [])
      pdef1 `shouldBe` pdef2
      pdef1 `shouldNotBe` pdef3

  describe "ProcBody" $ do
    it "creates ProcBody with no state" $ do
      let body = ProcBody Nothing []
      procState body `shouldBe` Nothing
      procReceive body `shouldBe` []

    it "creates ProcBody with state" $ do
      let initState = ELiteral (LInt 0)
      let body = ProcBody (Just initState) []
      procState body `shouldBe` Just initState

    it "creates ProcBody with receive cases" $ do
      let case1 = Case (PAtom (T.pack "msg")) (ELiteral (LInt 1))
      let body = ProcBody Nothing [case1]
      procReceive body `shouldBe` [case1]

    it "shows ProcBody correctly" $ do
      let body = ProcBody Nothing []
      show body `shouldContain` "ProcBody"

    it "compares ProcBodies for equality" $ do
      let body1 = ProcBody (Just (ELiteral (LInt 0))) []
      let body2 = ProcBody (Just (ELiteral (LInt 0))) []
      let body3 = ProcBody Nothing []
      body1 `shouldBe` body2
      body1 `shouldNotBe` body3

  describe "ReceiveCase" $ do
    it "creates simple receive case" $ do
      let pat = PAtom (T.pack "ok")
      let expr = ELiteral (LInt 1)
      let rcase = Case pat expr
      rcase `shouldBe` Case pat expr

    it "creates receive case with variable pattern" $ do
      let pat = PVar (T.pack "msg")
      let expr = ESend ESelf (EVar (T.pack "msg"))
      let rcase = Case pat expr
      rcase `shouldBe` Case pat expr

    it "shows ReceiveCase correctly" $ do
      let rcase = Case (PAtom (T.pack "test")) (ELiteral (LInt 5))
      show rcase `shouldContain` "Case"

    it "compares ReceiveCases for equality" $ do
      let case1 = Case (PVar (T.pack "x")) (ELiteral (LInt 1))
      let case2 = Case (PVar (T.pack "x")) (ELiteral (LInt 1))
      let case3 = Case (PVar (T.pack "y")) (ELiteral (LInt 1))
      case1 `shouldBe` case2
      case1 `shouldNotBe` case3

  describe "Pattern" $ do
    it "creates PVar pattern" $ do
      let pat = PVar (T.pack "x")
      pat `shouldBe` PVar (T.pack "x")

    it "creates PWildcard pattern" $ do
      let pat = PWildcard
      pat `shouldBe` PWildcard

    it "creates PLiteral pattern with int" $ do
      let pat = PLiteral (LInt 42)
      pat `shouldBe` PLiteral (LInt 42)

    it "creates PLiteral pattern with string" $ do
      let pat = PLiteral (LString (T.pack "hello"))
      pat `shouldBe` PLiteral (LString (T.pack "hello"))

    it "creates PAtom pattern" $ do
      let pat = PAtom (T.pack "ok")
      pat `shouldBe` PAtom (T.pack "ok")

    it "creates PTuple pattern" $ do
      let pat = PTuple [PVar (T.pack "x"), PVar (T.pack "y")]
      pat `shouldBe` PTuple [PVar (T.pack "x"), PVar (T.pack "y")]

    it "creates nested PTuple pattern" $ do
      let pat = PTuple [PVar (T.pack "a"), PTuple [PVar (T.pack "b"), PVar (T.pack "c")]]
      pat `shouldBe` PTuple [PVar (T.pack "a"), PTuple [PVar (T.pack "b"), PVar (T.pack "c")]]

    it "creates PVarargs pattern" $ do
      let pat = PVarargs (T.pack "rest")
      pat `shouldBe` PVarargs (T.pack "rest")

    it "shows patterns correctly" $ do
      show (PVar (T.pack "x")) `shouldContain` "PVar"
      show PWildcard `shouldContain` "PWildcard"
      show (PLiteral (LInt 5)) `shouldContain` "PLiteral"
      show (PAtom (T.pack "ok")) `shouldContain` "PAtom"
      show (PTuple []) `shouldContain` "PTuple"
      show (PVarargs (T.pack "rest")) `shouldContain` "PVarargs"

    it "compares patterns for equality" $ do
      PVar (T.pack "x") `shouldBe` PVar (T.pack "x")
      PVar (T.pack "x") `shouldNotBe` PVar (T.pack "y")
      PWildcard `shouldBe` PWildcard
      PLiteral (LInt 5) `shouldBe` PLiteral (LInt 5)
      PLiteral (LInt 5) `shouldNotBe` PLiteral (LInt 6)
      PAtom (T.pack "ok") `shouldBe` PAtom (T.pack "ok")
      PTuple [] `shouldBe` PTuple []
      PVarargs (T.pack "x") `shouldBe` PVarargs (T.pack "x")

  describe "Expr" $ do
    it "creates EVar expression" $ do
      let expr = EVar (T.pack "x")
      expr `shouldBe` EVar (T.pack "x")

    it "creates ELiteral expression" $ do
      let expr = ELiteral (LInt 42)
      expr `shouldBe` ELiteral (LInt 42)

    it "creates EAtom expression" $ do
      let expr = EAtom (T.pack "ok")
      expr `shouldBe` EAtom (T.pack "ok")

    it "creates ETuple expression" $ do
      let expr = ETuple [ELiteral (LInt 1), ELiteral (LInt 2)]
      expr `shouldBe` ETuple [ELiteral (LInt 1), ELiteral (LInt 2)]

    it "creates ECall expression" $ do
      let expr = ECall (T.pack "add") [EVar (T.pack "x"), EVar (T.pack "y")]
      expr `shouldBe` ECall (T.pack "add") [EVar (T.pack "x"), EVar (T.pack "y")]

    it "creates ESpawn expression" $ do
      let expr = ESpawn (T.pack "Counter") [ELiteral (LInt 0)]
      expr `shouldBe` ESpawn (T.pack "Counter") [ELiteral (LInt 0)]

    it "creates ESend expression" $ do
      let expr = ESend (EVar (T.pack "pid")) (EAtom (T.pack "msg"))
      expr `shouldBe` ESend (EVar (T.pack "pid")) (EAtom (T.pack "msg"))

    it "creates EAssign expression" $ do
      let expr = EAssign (T.pack "x") (ELiteral (LInt 10))
      expr `shouldBe` EAssign (T.pack "x") (ELiteral (LInt 10))

    it "creates EBlock expression" $ do
      let stmt = SLet (T.pack "x") Nothing (ELiteral (LInt 1))
      let expr = EBlock [stmt] (EVar (T.pack "x"))
      expr `shouldBe` EBlock [stmt] (EVar (T.pack "x"))

    it "creates EReceive expression" $ do
      let case1 = Case (PAtom (T.pack "ok")) (ELiteral (LInt 1))
      let expr = EReceive [case1]
      expr `shouldBe` EReceive [case1]

    it "creates EBinOp expression with Add" $ do
      let expr = EBinOp Add (ELiteral (LInt 1)) (ELiteral (LInt 2))
      expr `shouldBe` EBinOp Add (ELiteral (LInt 1)) (ELiteral (LInt 2))

    it "creates EBinOp expression with Sub" $ do
      let expr = EBinOp Sub (EVar (T.pack "x")) (ELiteral (LInt 1))
      expr `shouldBe` EBinOp Sub (EVar (T.pack "x")) (ELiteral (LInt 1))

    it "creates EBinOp expression with Mul" $ do
      let expr = EBinOp Mul (ELiteral (LInt 3)) (ELiteral (LInt 4))
      expr `shouldBe` EBinOp Mul (ELiteral (LInt 3)) (ELiteral (LInt 4))

    it "creates EBinOp expression with Div" $ do
      let expr = EBinOp Div (ELiteral (LInt 10)) (ELiteral (LInt 2))
      expr `shouldBe` EBinOp Div (ELiteral (LInt 10)) (ELiteral (LInt 2))

    it "creates EJust expression" $ do
      let expr = EJust (ELiteral (LInt 42))
      expr `shouldBe` EJust (ELiteral (LInt 42))

    it "creates ENone expression" $ do
      let expr = ENone
      expr `shouldBe` ENone

    it "creates ELeft expression (ko)" $ do
      let expr = ELeft (EAtom (T.pack "error"))
      expr `shouldBe` ELeft (EAtom (T.pack "error"))

    it "creates ERight expression (ok)" $ do
      let expr = ERight (ELiteral (LInt 100))
      expr `shouldBe` ERight (ELiteral (LInt 100))

    it "creates nested EJust expression" $ do
      let expr = EJust (EJust (ELiteral (LInt 5)))
      expr `shouldBe` EJust (EJust (ELiteral (LInt 5)))

    it "creates EJust with None inside" $ do
      let expr = EJust ENone
      expr `shouldBe` EJust ENone

    it "creates EJust with complex expression" $ do
      let expr = EJust (EBinOp Add (EVar (T.pack "x")) (ELiteral (LInt 1)))
      expr `shouldBe` EJust (EBinOp Add (EVar (T.pack "x")) (ELiteral (LInt 1)))

    it "creates ELeft with tuple" $ do
      let expr = ELeft (ETuple [EAtom (T.pack "error"), ELiteral (LString (T.pack "msg"))])
      expr `shouldBe` ELeft (ETuple [EAtom (T.pack "error"), ELiteral (LString (T.pack "msg"))])

    it "creates ERight with function call" $ do
      let expr = ERight (ECall (T.pack "compute") [EVar (T.pack "x")])
      expr `shouldBe` ERight (ECall (T.pack "compute") [EVar (T.pack "x")])

    it "creates Maybe of Either expression" $ do
      let expr = EJust (ERight (ELiteral (LInt 42)))
      expr `shouldBe` EJust (ERight (ELiteral (LInt 42)))

    it "creates Either of Maybe expression" $ do
      let expr = ELeft (EJust (ELiteral (LString (T.pack "wrapped"))))
      expr `shouldBe` ELeft (EJust (ELiteral (LString (T.pack "wrapped"))))

    it "shows Maybe expressions correctly" $ do
      show (EJust (ELiteral (LInt 1))) `shouldContain` "EJust"
      show ENone `shouldContain` "ENone"

    it "shows Either expressions correctly" $ do
      show (ELeft (ELiteral (LInt 1))) `shouldContain` "ELeft"
      show (ERight (ELiteral (LInt 2))) `shouldContain` "ERight"

    it "compares Maybe expressions for equality" $ do
      EJust (ELiteral (LInt 1)) `shouldBe` EJust (ELiteral (LInt 1))
      EJust (ELiteral (LInt 1)) `shouldNotBe` EJust (ELiteral (LInt 2))
      ENone `shouldBe` ENone

    it "compares Either expressions for equality" $ do
      ELeft (EVar (T.pack "x")) `shouldBe` ELeft (EVar (T.pack "x"))
      ELeft (EVar (T.pack "x")) `shouldNotBe` ELeft (EVar (T.pack "y"))
      ERight (EVar (T.pack "x")) `shouldBe` ERight (EVar (T.pack "x"))
      ERight (EVar (T.pack "x")) `shouldNotBe` ELeft (EVar (T.pack "x"))

  describe "Literal" $ do
    it "creates LInt literal" $ do
      let lit = LInt 42
      lit `shouldBe` LInt 42

    it "creates LString literal" $ do
      let lit = LString (T.pack "hello")
      lit `shouldBe` LString (T.pack "hello")

    it "creates LNone literal" $ do
      let lit = LNone
      lit `shouldBe` LNone

    it "shows literals correctly" $ do
      show (LInt 42) `shouldContain` "LInt"
      show (LString (T.pack "test")) `shouldContain` "LString"
      show LNone `shouldContain` "LNone"

    it "compares literals for equality" $ do
      LInt 5 `shouldBe` LInt 5
      LInt 5 `shouldNotBe` LInt 6
      LString (T.pack "a") `shouldBe` LString (T.pack "a")
      LString (T.pack "a") `shouldNotBe` LString (T.pack "b")
      LNone `shouldBe` LNone

  describe "Op" $ do
    it "creates all arithmetic operators" $ do
      Add `shouldBe` Add
      Sub `shouldBe` Sub
      Mul `shouldBe` Mul
      Div `shouldBe` Div

    it "creates Concat operator" $ do
      Concat `shouldBe` Concat

    it "creates all comparison operators" $ do
      Eq `shouldBe` Eq
      Neq `shouldBe` Neq
      Lt `shouldBe` Lt
      Gt `shouldBe` Gt
      Lte `shouldBe` Lte
      Gte `shouldBe` Gte

    it "creates all logical operators" $ do
      And `shouldBe` And
      Or `shouldBe` Or

    it "shows operators correctly" $ do
      show Add `shouldContain` "Add"
      show Sub `shouldContain` "Sub"
      show Mul `shouldContain` "Mul"
      show Div `shouldContain` "Div"
      show Concat `shouldContain` "Concat"
      show Eq `shouldContain` "Eq"
      show Neq `shouldContain` "Neq"
      show Lt `shouldContain` "Lt"
      show Gt `shouldContain` "Gt"
      show Lte `shouldContain` "Lte"
      show Gte `shouldContain` "Gte"
      show And `shouldContain` "And"
      show Or `shouldContain` "Or"

    it "compares operators for equality" $ do
      Add `shouldBe` Add
      Add `shouldNotBe` Sub
      Eq `shouldBe` Eq
      Eq `shouldNotBe` Neq

  describe "Stmt" $ do
    it "creates SLet statement" $ do
      let stmt = SLet (T.pack "x") Nothing (ELiteral (LInt 42))
      stmt `shouldBe` SLet (T.pack "x") Nothing (ELiteral (LInt 42))

    it "creates SAssign statement" $ do
      let stmt = SAssign (T.pack "x") (ELiteral (LInt 10))
      stmt `shouldBe` SAssign (T.pack "x") (ELiteral (LInt 10))

    it "creates SExpr statement" $ do
      let stmt = SExpr (ECall (T.pack "print") [ELiteral (LString (T.pack "hello"))])
      stmt `shouldBe` SExpr (ECall (T.pack "print") [ELiteral (LString (T.pack "hello"))])

    it "shows statements correctly" $ do
      show (SLet (T.pack "x") Nothing (ELiteral (LInt 1))) `shouldContain` "SLet"
      show (SAssign (T.pack "y") (ELiteral (LInt 2))) `shouldContain` "SAssign"
      show (SExpr (ELiteral (LInt 3))) `shouldContain` "SExpr"

    it "compares statements for equality" $ do
      SLet (T.pack "x") Nothing (ELiteral (LInt 1)) `shouldBe` SLet (T.pack "x") Nothing (ELiteral (LInt 1))
      SLet (T.pack "x") Nothing (ELiteral (LInt 1)) `shouldNotBe` SLet (T.pack "y") Nothing (ELiteral (LInt 1))
      SAssign (T.pack "a") (ELiteral (LInt 5)) `shouldBe` SAssign (T.pack "a") (ELiteral (LInt 5))
      SExpr (EVar (T.pack "x")) `shouldBe` SExpr (EVar (T.pack "x"))

  describe "Complex AST structures" $ do
    it "creates complex nested expression" $ do
      let expr =
            EBinOp
              Add
              (EBinOp Mul (ELiteral (LInt 2)) (ELiteral (LInt 3)))
              (ELiteral (LInt 4))
      expr `shouldBe` EBinOp Add (EBinOp Mul (ELiteral (LInt 2)) (ELiteral (LInt 3))) (ELiteral (LInt 4))

    it "creates function call with multiple arguments" $ do
      let expr = ECall (T.pack "foo") [EVar (T.pack "a"), EVar (T.pack "b"), ELiteral (LInt 5)]
      expr `shouldBe` ECall (T.pack "foo") [EVar (T.pack "a"), EVar (T.pack "b"), ELiteral (LInt 5)]

    it "creates nested blocks" $ do
      let innerBlock = EBlock [SLet (T.pack "y") Nothing (ELiteral (LInt 2))] (EVar (T.pack "y"))
      let outerBlock = EBlock [SLet (T.pack "x") Nothing innerBlock] (EVar (T.pack "x"))
      outerBlock `shouldBe` EBlock [SLet (T.pack "x") Nothing innerBlock] (EVar (T.pack "x"))

    it "creates procedure with receive cases" $ do
      let case1 = Case (PAtom (T.pack "inc")) (EBinOp Add (EVar (T.pack "state")) (ELiteral (LInt 1)))
      let case2 = Case (PAtom (T.pack "dec")) (EBinOp Sub (EVar (T.pack "state")) (ELiteral (LInt 1)))
      let body = ProcBody (Just (ELiteral (LInt 0))) [case1, case2]
      let pdef = ProcDef (T.pack "Counter") [] body
      pdef `shouldBe` ProcDef (T.pack "Counter") [] body

    it "creates complete program with multiple definitions" $ do
      let stmt1 = DStmt (SLet (T.pack "x") Nothing (ELiteral (LInt 1)))
      let pdef = ProcDef (T.pack "Test") [] (ProcBody Nothing [])
      let stmt2 = DProc pdef
      let prog = Program [stmt1, stmt2]
      prog `shouldBe` Program [stmt1, stmt2]

  describe "Edge cases" $ do
    it "handles empty tuple" $ do
      ETuple [] `shouldBe` ETuple []

    it "handles empty parameter list" $ do
      ECall (T.pack "f") [] `shouldBe` ECall (T.pack "f") []

    it "handles empty block" $ do
      EBlock [] (ELiteral (LInt 1)) `shouldBe` EBlock [] (ELiteral (LInt 1))

    it "handles large integer literals" $ do
      LInt 9999999999 `shouldBe` LInt 9999999999

    it "handles empty string" $ do
      LString (T.pack "") `shouldBe` LString (T.pack "")

  describe "Maybe and Either Types" $ do
    it "creates TMaybe type" $ do
      let maybeType = TMaybe (TNumeric I32)
      maybeType `shouldBe` TMaybe (TNumeric I32)

    it "creates nested TMaybe type" $ do
      let nestedMaybe = TMaybe (TMaybe (TNumeric I32))
      nestedMaybe `shouldBe` TMaybe (TMaybe (TNumeric I32))

    it "creates TEither type" $ do
      let eitherType = TEither (TNumeric I32) TString
      eitherType `shouldBe` TEither (TNumeric I32) TString

    it "creates complex Either type with tuples" $ do
      let complexEither = TEither (TTuple [TNumeric I32, TString]) (TNumeric I64)
      complexEither `shouldBe` TEither (TTuple [TNumeric I32, TString]) (TNumeric I64)

    it "creates Maybe of Either type" $ do
      let maybeEither = TMaybe (TEither (TNumeric I32) TString)
      maybeEither `shouldBe` TMaybe (TEither (TNumeric I32) TString)

    it "creates Either of Maybe type" $ do
      let eitherMaybe = TEither (TMaybe TString) (TNumeric I32)
      eitherMaybe `shouldBe` TEither (TMaybe TString) (TNumeric I32)

    it "shows Maybe type correctly" $ do
      show (TMaybe (TNumeric I32)) `shouldContain` "TMaybe"

    it "shows Either type correctly" $ do
      show (TEither (TNumeric I32) TString) `shouldContain` "TEither"

    it "compares Maybe types for equality" $ do
      TMaybe (TNumeric I32) `shouldBe` TMaybe (TNumeric I32)
      TMaybe (TNumeric I32) `shouldNotBe` TMaybe (TNumeric I64)

    it "compares Either types for equality" $ do
      TEither (TNumeric I32) TString `shouldBe` TEither (TNumeric I32) TString
      TEither (TNumeric I32) TString `shouldNotBe` TEither (TNumeric I64) TString

  describe "Maybe and Either Expressions" $ do
    it "creates EJust expression" $ do
      let expr = EJust (ELiteral (LInt 42))
      expr `shouldBe` EJust (ELiteral (LInt 42))

    it "creates ENone expression" $ do
      let expr = ENone
      expr `shouldBe` ENone

    it "creates ELeft expression" $ do
      let expr = ELeft (EAtom (T.pack "error"))
      expr `shouldBe` ELeft (EAtom (T.pack "error"))

    it "creates ERight expression" $ do
      let expr = ERight (ELiteral (LInt 100))
      expr `shouldBe` ERight (ELiteral (LInt 100))

    it "creates nested EJust expression" $ do
      let expr = EJust (EJust (ELiteral (LInt 5)))
      expr `shouldBe` EJust (EJust (ELiteral (LInt 5)))

    it "creates EJust with None inside" $ do
      let expr = EJust ENone
      expr `shouldBe` EJust ENone

    it "creates EJust with complex expression" $ do
      let expr = EJust (EBinOp Add (EVar (T.pack "x")) (ELiteral (LInt 1)))
      expr `shouldBe` EJust (EBinOp Add (EVar (T.pack "x")) (ELiteral (LInt 1)))

    it "creates ELeft with tuple" $ do
      let expr = ELeft (ETuple [EAtom (T.pack "error"), ELiteral (LString (T.pack "msg"))])
      expr `shouldBe` ELeft (ETuple [EAtom (T.pack "error"), ELiteral (LString (T.pack "msg"))])

    it "creates ERight with function call" $ do
      let expr = ERight (ECall (T.pack "compute") [EVar (T.pack "x")])
      expr `shouldBe` ERight (ECall (T.pack "compute") [EVar (T.pack "x")])

    it "creates Maybe of Either expression" $ do
      let expr = EJust (ERight (ELiteral (LInt 42)))
      expr `shouldBe` EJust (ERight (ELiteral (LInt 42)))

    it "creates Either of Maybe expression" $ do
      let expr = ELeft (EJust (ELiteral (LString (T.pack "wrapped"))))
      expr `shouldBe` ELeft (EJust (ELiteral (LString (T.pack "wrapped"))))

    it "shows Maybe expressions correctly" $ do
      show (EJust (ELiteral (LInt 1))) `shouldContain` "EJust"
      show ENone `shouldContain` "ENone"

    it "shows Either expressions correctly" $ do
      show (ELeft (ELiteral (LInt 1))) `shouldContain` "ELeft"
      show (ERight (ELiteral (LInt 2))) `shouldContain` "ERight"

    it "compares Maybe expressions for equality" $ do
      EJust (ELiteral (LInt 1)) `shouldBe` EJust (ELiteral (LInt 1))
      EJust (ELiteral (LInt 1)) `shouldNotBe` EJust (ELiteral (LInt 2))
      ENone `shouldBe` ENone

    it "compares Either expressions for equality" $ do
      ELeft (EVar (T.pack "x")) `shouldBe` ELeft (EVar (T.pack "x"))
      ELeft (EVar (T.pack "x")) `shouldNotBe` ELeft (EVar (T.pack "y"))
      ERight (EVar (T.pack "x")) `shouldBe` ERight (EVar (T.pack "x"))
      ERight (EVar (T.pack "x")) `shouldNotBe` ELeft (EVar (T.pack "x"))
