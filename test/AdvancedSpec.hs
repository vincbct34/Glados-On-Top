{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Additional edge case and advanced feature tests
-}
{-# LANGUAGE LambdaCase #-}

module AdvancedSpec (spec) where

import Data.Text (pack)
import Ratatouille.AST
import Ratatouille.Bytecode.Compiler (compileExpr)
import Ratatouille.Bytecode.Types
import Ratatouille.Parser.Proc (pProgram)
import Test.Hspec
import Text.Megaparsec (errorBundlePretty, parse)

spec :: Spec
spec = do
  describe "Advanced Features and Edge Cases" $ do
    typeSystemTests
    monadicTests
    castingTests
    incrementDecrementTests
    matchExpressionTests
    fieldAccessTests
    unaryOperatorTests
    stringOperationsTests
    commentTests

typeSystemTests :: Spec
typeSystemTests = describe "Type System" $ do
  it "compiles typed integer literals" $ do
    let expr = ELiteral (LTypedInt I32 42)
    compileExpr expr `shouldBe` [PUSH_INT 42]

  it "compiles typed float literals" $ do
    let expr = ELiteral (LTypedFloat F64 3.14)
    compileExpr expr `shouldBe` [PUSH_FLOAT 3.14]

  it "compiles typed variable declarations" $ do
    let expr = EBlock [SLet (pack "x") (Just (TNumeric I32)) (ELiteral (LInt 42))] (EVar (pack "x"))
    let bytecode = compileExpr expr
    length bytecode `shouldSatisfy` (> 0)

monadicTests :: Spec
monadicTests = describe "Monadic Operations" $ do
  it "compiles Just expressions" $ do
    let expr = EJust (ELiteral (LInt 42))
    compileExpr expr `shouldBe` [PUSH_INT 42, PUSH_JUST]

  it "compiles None expressions" $ do
    let expr = ENone
    compileExpr expr `shouldBe` [PUSH_NONE]

  it "compiles Left expressions" $ do
    let expr = ELeft (ELiteral (LString (pack "error")))
    compileExpr expr `shouldBe` [PUSH_STRING (pack "error"), PUSH_LEFT]

  it "compiles Right expressions" $ do
    let expr = ERight (ELiteral (LInt 42))
    compileExpr expr `shouldBe` [PUSH_INT 42, PUSH_RIGHT]

  it "compiles Maybe bind" $ do
    let expr = EMaybeBind (EVar (pack "m")) (EVar (pack "f"))
    let bytecode = compileExpr expr
    any (\case MAYBE_BIND _ -> True; _ -> False) bytecode `shouldBe` True

  it "compiles Either bind" $ do
    let expr = EEitherBind (EVar (pack "m")) (EVar (pack "f"))
    let bytecode = compileExpr expr
    any (\case EITHER_BIND _ -> True; _ -> False) bytecode `shouldBe` True

castingTests :: Spec
castingTests = describe "Type Casting" $ do
  it "compiles static cast" $ do
    let expr = ECast StaticCast (TNumeric I64) (ELiteral (LInt 42))
    let bytecode = compileExpr expr
    STATIC_CAST (pack "i64") `elem` bytecode `shouldBe` True

  it "compiles reinterpret cast" $ do
    let expr = ECast ReinterpretCast (TNumeric F32) (ELiteral (LInt 42))
    let bytecode = compileExpr expr
    REINTERPRET_CAST (pack "f32") `elem` bytecode `shouldBe` True

  it "compiles const cast" $ do
    let expr = ECast ConstCast (TNumeric I32) (EVar (pack "x"))
    let bytecode = compileExpr expr
    CONST_CAST `elem` bytecode `shouldBe` True

incrementDecrementTests :: Spec
incrementDecrementTests = describe "Increment/Decrement Operators" $ do
  it "compiles pre-increment" $ do
    let expr = EPreInc (pack "x")
    compileExpr expr `shouldBe` [INC_VAR (pack "x")]

  it "compiles post-increment" $ do
    let expr = EPostInc (pack "x")
    compileExpr expr `shouldBe` [INC_VAR_POST (pack "x")]

  it "compiles pre-decrement" $ do
    let expr = EPreDec (pack "x")
    compileExpr expr `shouldBe` [DEC_VAR (pack "x")]

  it "compiles post-decrement" $ do
    let expr = EPostDec (pack "x")
    compileExpr expr `shouldBe` [DEC_VAR_POST (pack "x")]

matchExpressionTests :: Spec
matchExpressionTests = describe "Match Expressions" $ do
  it "compiles simple match with literal patterns" $ do
    let expr =
          EMatch
            (EVar (pack "x"))
            [ MatchCase (PLiteral (LInt 1)) (ELiteral (LString (pack "one"))),
              MatchCase (PLiteral (LInt 2)) (ELiteral (LString (pack "two"))),
              MatchCase PWildcard (ELiteral (LString (pack "other")))
            ]
    let bytecode = compileExpr expr
    length bytecode `shouldSatisfy` (> 5)

  it "compiles match with tuple patterns" $ do
    let expr =
          EMatch
            (EVar (pack "pair"))
            [ MatchCase
                (PTuple [PVarTyped (pack "x") Nothing False, PVarTyped (pack "y") Nothing False])
                (EBinOp Add (EVar (pack "x")) (EVar (pack "y")))
            ]
    let bytecode = compileExpr expr
    any (\case MATCH_TUPLE _ _ -> True; _ -> False) bytecode `shouldBe` True

fieldAccessTests :: Spec
fieldAccessTests = describe "Field Access" $ do
  it "compiles field access on variable" $ do
    let expr = EFieldAccess (EVar (pack "obj")) (pack "field")
    let bytecode = compileExpr expr
    GET_FIELD (pack "field") `elem` bytecode `shouldBe` True

  it "compiles nested field access" $ do
    let expr = EFieldAccess (EFieldAccess (EVar (pack "obj")) (pack "inner")) (pack "field")
    let bytecode = compileExpr expr
    length bytecode `shouldSatisfy` (> 2)

unaryOperatorTests :: Spec
unaryOperatorTests = describe "Unary Operators" $ do
  it "compiles logical not" $ do
    let expr = EUnaryOp UNot (ELiteral (LBool True))
    let bytecode = compileExpr expr
    LOGIC_NOT `elem` bytecode `shouldBe` True

  it "compiles arithmetic negation" $ do
    let expr = EUnaryOp UNeg (ELiteral (LInt 42))
    let bytecode = compileExpr expr
    NEGATE `elem` bytecode `shouldBe` True

  it "compiles unary plus (noop)" $ do
    let expr = EUnaryOp UPlus (ELiteral (LInt 42))
    compileExpr expr `shouldBe` [PUSH_INT 42]

stringOperationsTests :: Spec
stringOperationsTests = describe "String Operations" $ do
  it "compiles string concatenation" $ do
    let expr = EBinOp Concat (ELiteral (LString (pack "Hello, "))) (ELiteral (LString (pack "World!")))
    let bytecode = compileExpr expr
    CONCAT `elem` bytecode `shouldBe` True

  it "compiles empty string" $ do
    let expr = ELiteral (LString (pack ""))
    compileExpr expr `shouldBe` [PUSH_STRING (pack "")]

  it "compiles string with escape sequences in AST" $ do
    let expr = ELiteral (LString (pack "Line 1\nLine 2\tTabbed"))
    let bytecode = compileExpr expr
    length bytecode `shouldBe` 1

commentTests :: Spec
commentTests = describe "Comment Handling" $ do
  it "parses program with single-line comments" $ do
    let source =
          pack $
            unlines
              [ "// This is a comment",
                "proc main() {",
                "  // Another comment",
                "  42  // Inline comment",
                "}"
              ]
    case parse pProgram "" source of
      Left err -> expectationFailure $ "Failed to parse: " ++ errorBundlePretty err
      Right (Program defs) -> length defs `shouldBe` 1

  it "parses program with multi-line comments" $ do
    let source =
          pack $
            unlines
              [ "/* This is a",
                "   multi-line comment */",
                "proc main() {",
                "  /* Simple comment */",
                "  42",
                "}"
              ]
    case parse pProgram "" source of
      Left err -> expectationFailure $ "Failed to parse: " ++ errorBundlePretty err
      Right (Program defs) -> length defs `shouldBe` 1
