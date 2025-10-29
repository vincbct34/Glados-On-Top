{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Parser.Pattern module tests
-}

module PatternParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text (pack)
import qualified Data.Text as T
import Ratatouille.AST
import Ratatouille.Parser.Pattern (pPattern, pTypedPattern, pReceiveCase)
import Ratatouille.Parser.Common (sc)

-- Helper functions for testing
shouldParsePatternAs :: T.Text -> Pattern -> Expectation
shouldParsePatternAs input expected =
  case parse (sc *> pPattern) "" input of
    Left err -> expectationFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right result -> result `shouldBe` expected

shouldParseTypedPatternAs :: T.Text -> Pattern -> Expectation  
shouldParseTypedPatternAs input expected =
  case parse (sc *> pTypedPattern) "" input of
    Left err -> expectationFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right result -> result `shouldBe` expected

shouldParseReceiveCaseAs :: T.Text -> ReceiveCase -> Expectation
shouldParseReceiveCaseAs input expected =
  case parse (sc *> pReceiveCase) "" input of
    Left err -> expectationFailure $ "Failed to parse: " ++ errorBundlePretty err
    Right result -> result `shouldBe` expected

shouldFailToParsePattern :: T.Text -> Expectation
shouldFailToParsePattern input =
  case parse (sc *> pPattern) "" input of
    Left _ -> return ()
    Right result -> expectationFailure $ "Expected parsing to fail, but got: " ++ show result

spec :: Spec
spec = describe "Parser.Pattern" $ do

  describe "pPattern (basic patterns)" $ do
    it "parses wildcard pattern _" $ do
      pack "_" `shouldParsePatternAs` PWildcard

    it "parses variable pattern" $ do
      pack "x" `shouldParsePatternAs` PVar (pack "x")
      pack "myVar" `shouldParsePatternAs` PVar (pack "myVar")
      pack "var_name" `shouldParsePatternAs` PVar (pack "var_name")

    it "parses atom pattern" $ do
      pack ":ok" `shouldParsePatternAs` PAtom (pack "ok")
      pack ":hello" `shouldParsePatternAs` PAtom (pack "hello")
      pack ":error_code" `shouldParsePatternAs` PAtom (pack "error_code")

    it "parses literal patterns" $ do
      pack "42" `shouldParsePatternAs` PLiteral (LInt 42)
      pack "-10" `shouldParsePatternAs` PLiteral (LInt (-10))
      pack "\"hello\"" `shouldParsePatternAs` PLiteral (LString (pack "hello"))
      pack "true" `shouldParsePatternAs` PLiteral (LBool True)
      pack "false" `shouldParsePatternAs` PLiteral (LBool False)
      pack "none" `shouldParsePatternAs` PLiteral LNone

    it "parses varargs pattern" $ do
      pack "rest..." `shouldParsePatternAs` PVarargs (pack "rest")
      pack "remaining..." `shouldParsePatternAs` PVarargs (pack "remaining")

    it "parses tuple patterns with 2 elements" $ do
      pack "(a, b)" `shouldParsePatternAs` PTuple [PVar (pack "a"), PVar (pack "b")]
      pack "(:ok, value)" `shouldParsePatternAs` PTuple [PAtom (pack "ok"), PVar (pack "value")]
      pack "(x, _)" `shouldParsePatternAs` PTuple [PVar (pack "x"), PWildcard]

    it "parses tuple patterns with more than 2 elements" $ do
      pack "(a, b, c)" `shouldParsePatternAs` PTuple [PVar (pack "a"), PVar (pack "b"), PVar (pack "c")]
      pack "(:add, x, y)" `shouldParsePatternAs` PTuple [PAtom (pack "add"), PVar (pack "x"), PVar (pack "y")]

    it "parses tuple patterns with trailing comma" $ do
      pack "(a, b,)" `shouldParsePatternAs` PTuple [PVar (pack "a"), PVar (pack "b")]

    it "parses parenthesized single pattern (not tuple)" $ do
      pack "(x)" `shouldParsePatternAs` PVar (pack "x")
      pack "(:ok)" `shouldParsePatternAs` PAtom (pack "ok")

    it "parses nested tuple patterns" $ do
      pack "(a, (b, c))" `shouldParsePatternAs` PTuple [PVar (pack "a"), PTuple [PVar (pack "b"), PVar (pack "c")]]

    it "handles whitespace correctly" $ do
      pack "  x  " `shouldParsePatternAs` PVar (pack "x")
      pack "( a , b )" `shouldParsePatternAs` PTuple [PVar (pack "a"), PVar (pack "b")]

    it "fails to parse invalid patterns" $ do
      shouldFailToParsePattern (pack "")
      shouldFailToParsePattern (pack "(")
      shouldFailToParsePattern (pack "(a")
      shouldFailToParsePattern (pack "(a,)")  -- Single element with comma should fail as tuple

  describe "pTypedPattern (typed patterns)" $ do
    it "parses basic patterns like pPattern" $ do
      pack "x" `shouldParseTypedPatternAs` PVar (pack "x")
      pack "_" `shouldParseTypedPatternAs` PWildcard
      pack ":ok" `shouldParseTypedPatternAs` PAtom (pack "ok")

    it "parses typed variable patterns" $ do
      pack "x<i32>" `shouldParseTypedPatternAs` PVarTyped (pack "x") (Just (TNumeric I32)) False
      pack "value<string>" `shouldParseTypedPatternAs` PVarTyped (pack "value") (Just TString) False
      pack "flag<bool>" `shouldParseTypedPatternAs` PVarTyped (pack "flag") (Just TBool) False

    it "parses typed patterns with complex types" $ do
      pack "data<i32?>" `shouldParseTypedPatternAs` PVarTyped (pack "data") (Just (TMaybe (TNumeric I32))) False
      pack "result<i32!string>" `shouldParseTypedPatternAs` PVarTyped (pack "result") (Just (TEither (TNumeric I32) TString)) False

    it "parses typed tuple patterns" $ do
      pack "(x<i32>, y<i32>)" `shouldParseTypedPatternAs` PTuple
        [ PVarTyped (pack "x") (Just (TNumeric I32)) False
        , PVarTyped (pack "y") (Just (TNumeric I32)) False
        ]

    it "parses mixed typed and untyped in tuples" $ do
      pack "(x<i32>, y, z<string>)" `shouldParseTypedPatternAs` PTuple
        [ PVarTyped (pack "x") (Just (TNumeric I32)) False
        , PVar (pack "y")
        , PVarTyped (pack "z") (Just TString) False
        ]

    it "handles whitespace in type annotations" $ do
      pack "x < i32 >" `shouldParseTypedPatternAs` PVarTyped (pack "x") (Just (TNumeric I32)) False

  describe "pReceiveCase (receive case patterns)" $ do
    it "parses simple receive case" $ do
      pack "| x -> x" `shouldParseReceiveCaseAs` Case (PVar (pack "x")) (EVar (pack "x"))

    it "parses receive case with atom pattern" $ do
      pack "| :ok -> 1" `shouldParseReceiveCaseAs` Case (PAtom (pack "ok")) (ELiteral (LInt 1))

    it "parses receive case with tuple pattern" $ do
      pack "| (x, y) -> x" `shouldParseReceiveCaseAs` Case 
        (PTuple [PVar (pack "x"), PVar (pack "y")]) 
        (EVar (pack "x"))

    it "parses receive case with typed patterns" $ do
      pack "| (cmd<atom>, value<i32>) -> value" `shouldParseReceiveCaseAs` Case
        (PTuple [
          PVarTyped (pack "cmd") (Just TAtom) False,
          PVarTyped (pack "value") (Just (TNumeric I32)) False
        ])
        (EVar (pack "value"))

    it "parses receive case with complex expressions" $ do
      pack "| x -> print(x)" `shouldParseReceiveCaseAs` Case 
        (PVar (pack "x")) 
        (ECall (pack "print") [EVar (pack "x")])

    it "handles whitespace around arrows" $ do
      pack "| x  ->  x" `shouldParseReceiveCaseAs` Case (PVar (pack "x")) (EVar (pack "x"))
      pack "|x->x" `shouldParseReceiveCaseAs` Case (PVar (pack "x")) (EVar (pack "x"))

  describe "Pattern parsing edge cases" $ do
    it "parses complex nested patterns" $ do
      pack "((:ok, (x, y)), z)" `shouldParsePatternAs` PTuple
        [ PTuple [PAtom (pack "ok"), PTuple [PVar (pack "x"), PVar (pack "y")]]
        , PVar (pack "z")
        ]

    it "handles different literal types in patterns" $ do
      pack "3.14" `shouldParsePatternAs` PLiteral (LFloat 3.14)
      pack "42i32" `shouldParseTypedPatternAs` PLiteral (LTypedInt I32 42)
      pack "3.14f32" `shouldParseTypedPatternAs` PLiteral (LTypedFloat F32 3.14)

    it "prioritizes varargs over regular identifiers" $ do
      pack "args..." `shouldParsePatternAs` PVarargs (pack "args")
      pack "args" `shouldParsePatternAs` PVar (pack "args")

    it "parses atoms with underscores" $ do
      pack ":error_code" `shouldParsePatternAs` PAtom (pack "error_code")
      pack ":my_atom_123" `shouldParsePatternAs` PAtom (pack "my_atom_123")