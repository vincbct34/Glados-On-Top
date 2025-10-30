{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Extended AST module tests for better coverage
-}

module ASTExtendedSpec (spec) where

import Test.Hspec
import Ratatouille.AST
import qualified Data.Text as T ()
import Data.Text (pack)

spec :: Spec
spec = describe "Extended AST Coverage" $ do

  describe "NumericType" $ do
    it "creates all signed integer types" $ do
      I8 `shouldBe` I8
      I16 `shouldBe` I16
      I32 `shouldBe` I32
      I64 `shouldBe` I64

    it "creates all unsigned integer types" $ do
      U8 `shouldBe` U8
      U16 `shouldBe` U16
      U32 `shouldBe` U32
      U64 `shouldBe` U64

    it "creates floating point types" $ do
      F32 `shouldBe` F32
      F64 `shouldBe` F64

    it "shows NumericType correctly" $ do
      show I32 `shouldContain` "I32"
      show F64 `shouldContain` "F64"

    it "compares NumericTypes for equality" $ do
      I32 `shouldBe` I32
      I32 `shouldNotBe` I64
      F32 `shouldBe` F32
      F32 `shouldNotBe` F64

  describe "Type" $ do
    it "creates numeric types" $ do
      TNumeric I32 `shouldBe` TNumeric I32
      TNumeric F64 `shouldBe` TNumeric F64

    it "creates basic types" $ do
      TString `shouldBe` TString
      TBool `shouldBe` TBool
      TAtom `shouldBe` TAtom
      TPid `shouldBe` TPid
      TNone `shouldBe` TNone
      TVoid `shouldBe` TVoid
      TAny `shouldBe` TAny

    it "creates complex types" $ do
      TMaybe TString `shouldBe` TMaybe TString
      TEither TString TBool `shouldBe` TEither TString TBool
      TTuple [TString, TBool] `shouldBe` TTuple [TString, TBool]
      TArray TString Nothing `shouldBe` TArray TString Nothing
      TArray TBool (Just 10) `shouldBe` TArray TBool (Just 10)

    it "shows Type correctly" $ do
      show TString `shouldContain` "TString"
      show (TMaybe TBool) `shouldContain` "TMaybe"
      show (TTuple [TString, TBool]) `shouldContain` "TTuple"

    it "compares Types for equality" $ do
      TString `shouldBe` TString
      TString `shouldNotBe` TBool
      TMaybe TString `shouldBe` TMaybe TString
      TMaybe TString `shouldNotBe` TMaybe TBool

  describe "CastType" $ do
    it "creates all cast types" $ do
      StaticCast `shouldBe` StaticCast
      ReinterpretCast `shouldBe` ReinterpretCast
      ConstCast `shouldBe` ConstCast

    it "shows CastType correctly" $ do
      show StaticCast `shouldContain` "StaticCast"
      show ReinterpretCast `shouldContain` "ReinterpretCast"
      show ConstCast `shouldContain` "ConstCast"

    it "compares CastTypes for equality" $ do
      StaticCast `shouldBe` StaticCast
      StaticCast `shouldNotBe` ReinterpretCast

  describe "Literal (extended coverage)" $ do
    it "creates typed literals" $ do
      LTypedInt I32 42 `shouldBe` LTypedInt I32 42
      LTypedFloat F64 3.14 `shouldBe` LTypedFloat F64 3.14

    it "shows typed literals correctly" $ do
      show (LTypedInt I32 42) `shouldContain` "LTypedInt"
      show (LTypedFloat F32 3.14) `shouldContain` "LTypedFloat"

    it "compares typed literals for equality" $ do
      LTypedInt I32 42 `shouldBe` LTypedInt I32 42
      LTypedInt I32 42 `shouldNotBe` LTypedInt I64 42
      LTypedFloat F32 3.14 `shouldBe` LTypedFloat F32 3.14

  describe "Pattern (extended coverage)" $ do
    it "creates PVarTyped patterns" $ do
      let pattern = PVarTyped (pack "x") (Just TString) False
      pattern `shouldBe` PVarTyped (pack "x") (Just TString) False

    it "creates PVarTyped with const flag" $ do
      let pattern = PVarTyped (pack "x") (Just TString) True
      pattern `shouldBe` PVarTyped (pack "x") (Just TString) True

    it "creates PArray patterns" $ do
      let pattern = PArray [PVar (pack "x"), PVar (pack "y")]
      pattern `shouldBe` PArray [PVar (pack "x"), PVar (pack "y")]

    it "creates PVarargs patterns" $ do
      PVarargs (pack "rest") `shouldBe` PVarargs (pack "rest")

    it "shows Pattern correctly" $ do
      show (PVarTyped (pack "x") Nothing False) `shouldContain` "PVarTyped"
      show (PArray [PWildcard]) `shouldContain` "PArray"
      show (PVarargs (pack "rest")) `shouldContain` "PVarargs"

  describe "Expr (extended coverage)" $ do
    it "creates array expressions" $ do
      EArray [ELiteral (LInt 1), ELiteral (LInt 2)] `shouldBe` 
        EArray [ELiteral (LInt 1), ELiteral (LInt 2)]

    it "creates index expressions" $ do
      EIndex (EVar (pack "arr")) (ELiteral (LInt 0)) `shouldBe`
        EIndex (EVar (pack "arr")) (ELiteral (LInt 0))

    it "creates cast expressions" $ do
      ECast StaticCast TString (EVar (pack "x")) `shouldBe`
        ECast StaticCast TString (EVar (pack "x"))

    it "creates increment/decrement expressions" $ do
      EPreInc (pack "x") `shouldBe` EPreInc (pack "x")
      EPostInc (pack "x") `shouldBe` EPostInc (pack "x")
      EPreDec (pack "x") `shouldBe` EPreDec (pack "x")
      EPostDec (pack "x") `shouldBe` EPostDec (pack "x")

    it "creates Maybe expressions" $ do
      EJust (ELiteral (LInt 42)) `shouldBe` EJust (ELiteral (LInt 42))
      ENone `shouldBe` ENone

    it "creates Either expressions" $ do
      ELeft (ELiteral (LString (pack "error"))) `shouldBe` ELeft (ELiteral (LString (pack "error")))
      ERight (ELiteral (LInt 42)) `shouldBe` ERight (ELiteral (LInt 42))

    it "creates monad bind expressions" $ do
      EMaybeBind (EVar (pack "x")) (EVar (pack "f")) `shouldBe`
        EMaybeBind (EVar (pack "x")) (EVar (pack "f"))
      EEitherBind (EVar (pack "x")) (EVar (pack "f")) `shouldBe`
        EEitherBind (EVar (pack "x")) (EVar (pack "f"))

    it "creates field access expressions" $ do
      EFieldAccess (EVar (pack "obj")) (pack "field") `shouldBe`
        EFieldAccess (EVar (pack "obj")) (pack "field")

    it "creates self expressions" $ do
      ESelf `shouldBe` ESelf

    it "shows Expr correctly" $ do
      show (EArray []) `shouldContain` "EArray"
      show (EIndex (EVar (pack "x")) (ELiteral (LInt 0))) `shouldContain` "EIndex"
      show (EPreInc (pack "x")) `shouldContain` "EPreInc"
      show ENone `shouldContain` "ENone"
      show ESelf `shouldContain` "ESelf"

  describe "Stmt (extended coverage)" $ do
    it "creates SLetPattern statements" $ do
      SLetPattern (PVar (pack "x")) (ELiteral (LInt 42)) `shouldBe`
        SLetPattern (PVar (pack "x")) (ELiteral (LInt 42))

    it "creates SConst statements" $ do
      SConst (pack "PI") (Just TString) (ELiteral (LFloat 3.14)) `shouldBe`
        SConst (pack "PI") (Just TString) (ELiteral (LFloat 3.14))

    it "shows Stmt correctly" $ do
      show (SLetPattern PWildcard (EVar (pack "x"))) `shouldContain` "SLetPattern"
      show (SConst (pack "x") Nothing (ELiteral (LInt 1))) `shouldContain` "SConst"

  describe "Definition (extended coverage)" $ do
    it "shows Definition correctly" $ do
      let stmt = SExpr (ELiteral (LInt 42))
      show (DStmt stmt) `shouldContain` "DStmt"

  describe "ProcDefinition (extended coverage)" $ do
    it "creates proc with empty body" $ do
      let procDef = ProcDef (pack "Empty") [] (ProcBody Nothing [])
      procDef `shouldBe` ProcDef (pack "Empty") [] (ProcBody Nothing [])

    it "creates proc with state and receive cases" $ do
      let state = ELiteral (LInt 0)
      let cases = [Case (PVar (pack "x")) (EVar (pack "x"))]
      let procDef = ProcDef (pack "Counter") [pack "initial"] (ProcBody (Just state) cases)
      procDef `shouldBe` ProcDef (pack "Counter") [pack "initial"] (ProcBody (Just state) cases)

  describe "ProcBody (extended coverage)" $ do
    it "creates body with no state" $ do
      let body = ProcBody Nothing []
      body `shouldBe` ProcBody Nothing []

    it "creates body with state but no receive cases" $ do
      let body = ProcBody (Just (ELiteral (LInt 0))) []
      body `shouldBe` ProcBody (Just (ELiteral (LInt 0))) []

  describe "Op (extended coverage)" $ do
    it "shows all operators correctly" $ do
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

    it "compares all operators for equality" $ do
      Add `shouldBe` Add
      Add `shouldNotBe` Sub
      And `shouldBe` And
      And `shouldNotBe` Or

  describe "Edge cases and complex structures" $ do
    it "handles deeply nested types" $ do
      let complexType = TMaybe (TEither (TArray TString Nothing) (TTuple [TNumeric I32, TBool]))
      complexType `shouldBe` TMaybe (TEither (TArray TString Nothing) (TTuple [TNumeric I32, TBool]))

    it "handles deeply nested expressions" $ do
      let complexExpr = EBinOp Add 
            (ECall (pack "func") [EVar (pack "x"), EVar (pack "y")])
            (EIndex (EArray [ELiteral (LInt 1), ELiteral (LInt 2)]) (ELiteral (LInt 0)))
      complexExpr `shouldBe` EBinOp Add 
        (ECall (pack "func") [EVar (pack "x"), EVar (pack "y")])
        (EIndex (EArray [ELiteral (LInt 1), ELiteral (LInt 2)]) (ELiteral (LInt 0)))

    it "handles complex patterns" $ do
      let complexPattern = PTuple [
            PArray [PVar (pack "x"), PWildcard],
            PVarTyped (pack "y") (Just (TMaybe TString)) False,
            PVarargs (pack "rest")
            ]
      complexPattern `shouldBe` PTuple [
        PArray [PVar (pack "x"), PWildcard],
        PVarTyped (pack "y") (Just (TMaybe TString)) False,
        PVarargs (pack "rest")
        ]

    it "handles empty collections" $ do
      EArray [] `shouldBe` EArray []
      ETuple [] `shouldBe` ETuple []
      TTuple [] `shouldBe` TTuple []
      PTuple [] `shouldBe` PTuple []

    it "handles programs with mixed definitions" $ do
      let stmt1 = DStmt (SLet (pack "x") Nothing (ELiteral (LInt 42)))
      let proc1 = DProc (ProcDef (pack "Test") [] (ProcBody Nothing []))
      let program = Program [stmt1, proc1]
      program `shouldBe` Program [stmt1, proc1]