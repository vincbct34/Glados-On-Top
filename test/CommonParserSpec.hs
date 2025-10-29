{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Parser.Common module tests
-}

module CommonParserSpec (spec) where

import Test.Hspec
import Text.Megaparsec (parse, errorBundlePretty)
import Data.Text (pack)
import qualified Data.Text as T
import Ratatouille.AST
import Ratatouille.Parser.Common

-- Helper functions for testing
shouldParseAs :: (Show a, Eq a) => T.Text -> (T.Text -> Either String a) -> a -> Expectation
shouldParseAs input parser expected =
  case parser input of
    Left err -> expectationFailure $ "Failed to parse: " ++ err
    Right result -> result `shouldBe` expected

parseIdentifier :: T.Text -> Either String T.Text
parseIdentifier input =
  case parse (sc *> pIdentifier) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result

parseLiteral :: T.Text -> Either String Literal
parseLiteral input =
  case parse (sc *> pLiteral) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result

parseAtom :: T.Text -> Either String Expr
parseAtom input =
  case parse (sc *> pAtom) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result

parseType :: T.Text -> Either String Type
parseType input =
  case parse (sc *> pType) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result

parseReturnType :: T.Text -> Either String Type
parseReturnType input =
  case parse (sc *> pReturnType) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result

parseNumericType :: T.Text -> Either String NumericType
parseNumericType input =
  case parse (sc *> pNumericType) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result

shouldFailToParse :: (Show a) => T.Text -> (T.Text -> Either String a) -> Expectation
shouldFailToParse input parser =
  case parser input of
    Left _ -> return ()
    Right result -> expectationFailure $ "Expected parsing to fail, but got: " ++ show result

spec :: Spec
spec = describe "Parser.Common" $ do

  describe "pIdentifier" $ do
    it "parses simple identifiers" $ do
      pack "hello" `shouldParseAs` parseIdentifier $ pack "hello"
      pack "world" `shouldParseAs` parseIdentifier $ pack "world"
      pack "myVar" `shouldParseAs` parseIdentifier $ pack "myVar"

    it "parses identifiers with underscores" $ do
      pack "_private" `shouldParseAs` parseIdentifier $ pack "_private"
      pack "var_name" `shouldParseAs` parseIdentifier $ pack "var_name"
      pack "my_long_variable_name" `shouldParseAs` parseIdentifier $ pack "my_long_variable_name"

    it "parses identifiers with numbers" $ do
      pack "var1" `shouldParseAs` parseIdentifier $ pack "var1"
      pack "test123" `shouldParseAs` parseIdentifier $ pack "test123"
      pack "x2y3z" `shouldParseAs` parseIdentifier $ pack "x2y3z"

    it "rejects reserved words" $ do
      shouldFailToParse (pack "proc") parseIdentifier
      shouldFailToParse (pack "receive") parseIdentifier
      shouldFailToParse (pack "spawn") parseIdentifier
      shouldFailToParse (pack "let") parseIdentifier
      shouldFailToParse (pack "const") parseIdentifier
      shouldFailToParse (pack "if") parseIdentifier
      shouldFailToParse (pack "then") parseIdentifier
      shouldFailToParse (pack "else") parseIdentifier
      shouldFailToParse (pack "self") parseIdentifier
      shouldFailToParse (pack "none") parseIdentifier
      shouldFailToParse (pack "void") parseIdentifier
      shouldFailToParse (pack "true") parseIdentifier
      shouldFailToParse (pack "false") parseIdentifier

    it "handles whitespace correctly" $ do
      pack "  hello  " `shouldParseAs` parseIdentifier $ pack "hello"

    it "rejects identifiers starting with numbers" $ do
      shouldFailToParse (pack "123abc") parseIdentifier
      shouldFailToParse (pack "1var") parseIdentifier

  describe "pLiteral" $ do
    it "parses integer literals" $ do
      pack "42" `shouldParseAs` parseLiteral $ LInt 42
      pack "-10" `shouldParseAs` parseLiteral $ LInt (-10)
      pack "0" `shouldParseAs` parseLiteral $ LInt 0

    it "parses typed integer literals" $ do
      pack "42i32" `shouldParseAs` parseLiteral $ LTypedInt I32 42
      pack "100u8" `shouldParseAs` parseLiteral $ LTypedInt U8 100
      pack "-5i64" `shouldParseAs` parseLiteral $ LTypedInt I64 (-5)

    it "parses float literals" $ do
      pack "3.14" `shouldParseAs` parseLiteral $ LFloat 3.14
      pack "-2.5" `shouldParseAs` parseLiteral $ LFloat (-2.5)
      pack "0.0" `shouldParseAs` parseLiteral $ LFloat 0.0

    it "parses typed float literals" $ do
      pack "3.14f32" `shouldParseAs` parseLiteral $ LTypedFloat F32 3.14
      pack "2.718f64" `shouldParseAs` parseLiteral $ LTypedFloat F64 2.718

    it "parses string literals" $ do
      pack "\"hello\"" `shouldParseAs` parseLiteral $ LString (pack "hello")
      pack "\"world\"" `shouldParseAs` parseLiteral $ LString (pack "world")
      pack "\"\"" `shouldParseAs` parseLiteral $ LString (pack "")

    it "parses string literals with escape sequences" $ do
      pack "\"hello\\nworld\"" `shouldParseAs` parseLiteral $ LString (pack "hello\nworld")
      pack "\"tab\\there\"" `shouldParseAs` parseLiteral $ LString (pack "tab\there")

    it "parses boolean literals" $ do
      pack "true" `shouldParseAs` parseLiteral $ LBool True
      pack "false" `shouldParseAs` parseLiteral $ LBool False

    it "parses none literal" $ do
      pack "none" `shouldParseAs` parseLiteral $ LNone

    it "prioritizes float over integer for decimal numbers" $ do
      pack "3.0" `shouldParseAs` parseLiteral $ LFloat 3.0

  describe "pAtom" $ do
    it "parses simple atoms" $ do
      pack ":ok" `shouldParseAs` parseAtom $ EAtom (pack "ok")
      pack ":error" `shouldParseAs` parseAtom $ EAtom (pack "error")
      pack ":hello" `shouldParseAs` parseAtom $ EAtom (pack "hello")

    it "parses atoms with underscores and numbers" $ do
      pack ":error_code" `shouldParseAs` parseAtom $ EAtom (pack "error_code")
      pack ":msg123" `shouldParseAs` parseAtom $ EAtom (pack "msg123")
      pack ":_private" `shouldParseAs` parseAtom $ EAtom (pack "_private")

  describe "pNumericType" $ do
    it "parses signed integer types" $ do
      pack "i8" `shouldParseAs` parseNumericType $ I8
      pack "i16" `shouldParseAs` parseNumericType $ I16
      pack "i32" `shouldParseAs` parseNumericType $ I32
      pack "i64" `shouldParseAs` parseNumericType $ I64

    it "parses unsigned integer types" $ do
      pack "u8" `shouldParseAs` parseNumericType $ U8
      pack "u16" `shouldParseAs` parseNumericType $ U16
      pack "u32" `shouldParseAs` parseNumericType $ U32
      pack "u64" `shouldParseAs` parseNumericType $ U64

    it "parses floating point types" $ do
      pack "f32" `shouldParseAs` parseNumericType $ F32
      pack "f64" `shouldParseAs` parseNumericType $ F64

  describe "pType" $ do
    it "parses simple types" $ do
      pack "i32" `shouldParseAs` parseType $ TNumeric I32
      pack "string" `shouldParseAs` parseType $ TString
      pack "bool" `shouldParseAs` parseType $ TBool
      pack "pid" `shouldParseAs` parseType $ TPid
      pack "atom" `shouldParseAs` parseType $ TAtom
      pack "none" `shouldParseAs` parseType $ TNone

    it "parses auto/any types" $ do
      pack "auto" `shouldParseAs` parseType $ TAny
      pack "any" `shouldParseAs` parseType $ TAny

    it "parses Maybe types" $ do
      pack "i32?" `shouldParseAs` parseType $ TMaybe (TNumeric I32)
      pack "string?" `shouldParseAs` parseType $ TMaybe TString

    it "parses Either types" $ do
      pack "i32!string" `shouldParseAs` parseType $ TEither (TNumeric I32) TString
      pack "bool!atom" `shouldParseAs` parseType $ TEither TBool TAtom

    it "parses nested Maybe types with parentheses" $ do
      pack "(i32?)?" `shouldParseAs` parseType $ TMaybe (TMaybe (TNumeric I32))
      pack "(string?)?" `shouldParseAs` parseType $ TMaybe (TMaybe TString)

    it "parses nested Either types with parentheses" $ do
      pack "(i32!string)?" `shouldParseAs` parseType $ TMaybe (TEither (TNumeric I32) TString)

    it "parses array types" $ do
      pack "[i32]" `shouldParseAs` parseType $ TArray (TNumeric I32) Nothing
      pack "[string]" `shouldParseAs` parseType $ TArray TString Nothing

    it "parses fixed-size array types" $ do
      pack "[i32, 10]" `shouldParseAs` parseType $ TArray (TNumeric I32) (Just 10)
      pack "[bool, 5]" `shouldParseAs` parseType $ TArray TBool (Just 5)

    it "parses tuple types" $ do
      pack "(i32, string)" `shouldParseAs` parseType $ TTuple [TNumeric I32, TString]
      pack "(bool, i32, string)" `shouldParseAs` parseType $ TTuple [TBool, TNumeric I32, TString]

    it "parses parenthesized types (not tuples)" $ do
      pack "(i32)" `shouldParseAs` parseType $ TNumeric I32
      pack "(string?)" `shouldParseAs` parseType $ TMaybe TString

    it "handles whitespace correctly" $ do
      pack "  i32  " `shouldParseAs` parseType $ TNumeric I32
      pack "( i32 , string )" `shouldParseAs` parseType $ TTuple [TNumeric I32, TString]

  describe "pReturnType" $ do
    it "parses regular types like pType" $ do
      pack "i32" `shouldParseAs` parseReturnType $ TNumeric I32
      pack "string" `shouldParseAs` parseReturnType $ TString

    it "parses void type (only allowed in return types)" $ do
      pack "void" `shouldParseAs` parseReturnType $ TVoid

    it "parses complex return types" $ do
      pack "i32?" `shouldParseAs` parseReturnType $ TMaybe (TNumeric I32)
      pack "(i32, string)" `shouldParseAs` parseReturnType $ TTuple [TNumeric I32, TString]

  describe "reservedWords" $ do
    it "contains all expected reserved words" $ do
      reservedWords `shouldContain` [pack "proc"]
      reservedWords `shouldContain` [pack "receive"]
      reservedWords `shouldContain` [pack "spawn"]
      reservedWords `shouldContain` [pack "let"]
      reservedWords `shouldContain` [pack "const"]
      reservedWords `shouldContain` [pack "if"]
      reservedWords `shouldContain` [pack "then"]
      reservedWords `shouldContain` [pack "else"]
      reservedWords `shouldContain` [pack "self"]
      reservedWords `shouldContain` [pack "none"]
      reservedWords `shouldContain` [pack "void"]
      reservedWords `shouldContain` [pack "true"]
      reservedWords `shouldContain` [pack "false"]

  describe "isIdentifierChar" $ do
    it "accepts alphanumeric and underscore" $ do
      isIdentifierChar 'a' `shouldBe` True
      isIdentifierChar 'Z' `shouldBe` True
      isIdentifierChar '1' `shouldBe` True
      isIdentifierChar '_' `shouldBe` True

    it "rejects special characters" $ do
      isIdentifierChar '!' `shouldBe` False
      isIdentifierChar '@' `shouldBe` False
      isIdentifierChar '#' `shouldBe` False
      isIdentifierChar ' ' `shouldBe` False