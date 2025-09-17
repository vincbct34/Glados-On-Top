module ParserSpec (spec) where

import Test.Hspec
import Parser

spec :: Spec
spec = do
  ---------------------------------------------------------------------------
  -- parseSatisfy
  ---------------------------------------------------------------------------
  describe "parseSatisfy" $ do
    it "parses a matching character" $ do
      runParser (parseSatisfy (== 'a')) "abc" `shouldBe` Right ('a', "bc")

    it "fails on non-matching character" $ do
      runParser (parseSatisfy (== 'a')) "xbc"
        `shouldBe` Left (PError (UnexpectedChar 'x' Nothing))

    it "fails on empty input" $ do
      runParser (parseSatisfy (== 'a')) ""
        `shouldBe` Left (PError (UnexpectedEOF Nothing))

  ---------------------------------------------------------------------------
  -- withError
  ---------------------------------------------------------------------------
  describe "withError" $ do
    it "overrides the error with a custom one" $ do
      runParser (withError (parseChar 'a') (GenericError "oops")) "x"
        `shouldBe` Left (GenericError "oops")

    it "passes through on success" $ do
      runParser (withError (parseChar 'a') (GenericError "oops")) "abc"
        `shouldBe` Right ('a', "bc")

  ---------------------------------------------------------------------------
  -- parseChar / parseCharCase
  ---------------------------------------------------------------------------
  describe "parseChar" $ do
    it "parses the expected char" $ do
      runParser (parseChar 'a') "abc" `shouldBe` Right ('a', "bc")

    it "fails on unexpected char" $ do
      runParser (parseChar 'a') "xbc"
        `shouldBe` Left (PError (ExpectedChar 'a' Nothing))

  describe "parseCharCase" $ do
    it "parses case-insensitive char" $ do
      runParser (parseCharCase 'a') "Abc" `shouldBe` Right ('A', "bc")

    it "fails if not matching ignoring case" $ do
      runParser (parseCharCase 'a') "Xbc"
        `shouldBe` Left (PError (UnexpectedChar 'X' Nothing))

  ---------------------------------------------------------------------------
  -- parseAnyChar / parseAnyCharCase
  ---------------------------------------------------------------------------
  describe "parseAnyChar" $ do
    it "parses if char is in set" $ do
      runParser (parseAnyChar "abc") "apple" `shouldBe` Right ('a', "pple")

    it "fails if char not in set" $ do
      runParser (parseAnyChar "abc") "z" `shouldBe` Left (PError (UnexpectedChar 'z' Nothing))

  describe "parseAnyCharCase" $ do
    it "parses if char is in set (case-insensitive)" $ do
      runParser (parseAnyCharCase "abc") "Bla" `shouldBe` Right ('B', "la")

    it "fails if char not in set" $ do
      runParser (parseAnyCharCase "abc") "Z"
        `shouldBe` Left (PError (UnexpectedChar 'Z' Nothing))

  ---------------------------------------------------------------------------
  -- parseAnd / parseAndWith
  ---------------------------------------------------------------------------
  describe "parseAnd" $ do
    it "parses two values in sequence" $ do
      runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abc"
        `shouldBe` Right (('a','b'), "c")

  describe "parseAndWith" $ do
    it "combines results with a function" $ do
      runParser (parseAndWith (\a b -> [a, b]) (parseChar 'a') (parseChar 'b')) "bcd"
        `shouldBe` Left (PError (ExpectedChar 'a' Nothing))

  ---------------------------------------------------------------------------
  -- parseMany / parseSome
  ---------------------------------------------------------------------------
  describe "parseMany" $ do
    it "parses zero occurrences" $ do
      runParser (parseMany (parseChar 'a')) "bbb"
        `shouldBe` Right ("", "bbb")

    it "parses multiple occurrences" $ do
      runParser (parseMany (parseChar 'a')) "aaabc"
        `shouldBe` Right ("aaa", "bc")

  describe "parseSome" $ do
    it "fails if no occurrence" $ do
      runParser (parseSome (parseChar 'a')) "bbb"
        `shouldBe` Left (PError (ExpectedChar 'a' Nothing))

    it "parses one or more occurrences" $ do
      runParser (parseSome (parseChar 'a')) "aaabc"
        `shouldBe` Right ("aaa", "bc")

  ---------------------------------------------------------------------------
  -- parseUInt / parseInt
  ---------------------------------------------------------------------------
  describe "parseUInt" $ do
    it "parses unsigned integers" $ do
      runParser parseUInt "123abc" `shouldBe` Right (123, "abc")

    it "fails on letters" $ do
      runParser parseUInt "abc" `shouldBe`
        Left (GenericError "Expected unsigned integer")

  describe "parseInt" $ do
    it "parses signed negative integers" $ do
      runParser parseInt "-42xyz" `shouldBe` Right (-42, "xyz")

    it "parses signed positive integers" $ do
      runParser parseInt "+7!" `shouldBe` Right (7, "!")

    it "parses plain integers" $ do
      runParser parseInt "99end" `shouldBe` Right (99, "end")

  ---------------------------------------------------------------------------
  -- parseRight / parseLeft
  ---------------------------------------------------------------------------
  describe "parseRight" $ do
    it "returns right parser result" $ do
      runParser (parseRight (parseChar 'a') (parseChar 'b')) "abc"
        `shouldBe` Right ('b', "c")

  describe "parseLeft" $ do
    it "returns left parser result" $ do
      runParser (parseLeft (parseChar 'a') (parseChar 'b')) "abc"
        `shouldBe` Right ('a', "c")

  ---------------------------------------------------------------------------
  -- Error utilities
  ---------------------------------------------------------------------------
  describe "withContext" $ do
    it "adds context to GenericError" $ do
      withContext "ctx" (GenericError "oops")
        `shouldBe` GenericError "ctx: oops"

  describe "chainErrors" $ do
    it "returns single error unchanged" $ do
      chainErrors [GenericError "one"] `shouldBe` GenericError "one"

    it "combines multiple errors" $ do
      case chainErrors [GenericError "a", GenericError "b"] of
        GenericError msg -> msg `shouldContain` "Multiple errors"
        _ -> expectationFailure "Expected GenericError"

    it "returns unknown error on empty list" $ do
      chainErrors [] `shouldBe` GenericError "Unknown error"

    it "combines errors of different types" $ do
      let e1 = GenericError "foo"
          e2 = PError (UnexpectedChar 'x' Nothing)
      case chainErrors [e1, e2] of
        GenericError msg -> do
          msg `shouldContain` "foo"
          msg `shouldContain` "unexpected character"
        _ -> expectationFailure "Expected GenericError"

  ---------------------------------------------------------------------------
  -- Show instances for errors
  ---------------------------------------------------------------------------
  describe "Show instances for errors" $ do
    it "shows ParseError with location" $ do
      let err = PError (UnexpectedChar 'x' (Just (SourceLocation 2 3 5)))
      show err `shouldContain` "line 2, column 3"

    it "shows SemanticError with location" $ do
      let err = SError (UndefinedVariable "foo" (Just (SourceLocation 1 2 3)))
      show err `shouldContain` "undefined variable 'foo'"

    it "shows RuntimeError with location" $ do
      let err = RError (RuntimeError "fail" (Just (SourceLocation 4 5 6)))
      show err `shouldContain` "Runtime error at line 4, column 5"

    it "shows GenericError" $ do
      let err = GenericError "msg"
      show err `shouldContain` "msg"

  ---------------------------------------------------------------------------
  -- Cas limites et erreurs supplémentaires
  ---------------------------------------------------------------------------
  describe "parseCharCase (edge cases)" $ do
    it "fails on empty input" $ do
      runParser (parseCharCase 'a') "" `shouldBe` Left (PError (UnexpectedEOF Nothing))

  describe "parseAnyCharCase (edge cases)" $ do
    it "fails on empty input" $ do
      runParser (parseAnyCharCase "abc") "" `shouldBe` Left (PError (UnexpectedEOF Nothing))

  describe "parseAnd (failures)" $ do
    it "fails if first parser fails" $ do
      runParser (parseAnd (parseChar 'a') (parseChar 'b')) "xbc"
        `shouldBe` Left (PError (ExpectedChar 'a' Nothing))
    it "fails if second parser fails" $ do
      runParser (parseAnd (parseChar 'a') (parseChar 'b')) "axc"
        `shouldBe` Left (PError (ExpectedChar 'b' Nothing))

  describe "parseAndWith (failures)" $ do
    it "fails if second parser fails" $ do
      runParser (parseAndWith (\a b -> [a, b]) (parseChar 'a') (parseChar 'b')) "axc"
        `shouldBe` Left (PError (ExpectedChar 'b' Nothing))

  describe "parseMany (edge)" $ do
    it "parses until parser fails" $ do
      runParser (parseMany (parseChar 'a')) "aaabbb"
        `shouldBe` Right ("aaa", "bbb")

  describe "parseSome (single occurrence)" $ do
    it "parses single occurrence" $ do
      runParser (parseSome (parseChar 'a')) "abbb"
        `shouldBe` Right ("a", "bbb")

  describe "parseUInt (failures)" $ do
    it "fails on empty input" $ do
      runParser parseUInt "" `shouldBe` Left (GenericError "Expected unsigned integer")
    it "fails on special chars" $ do
      runParser parseUInt "!@#" `shouldBe` Left (GenericError "Expected unsigned integer")

  describe "parseInt (failures)" $ do
    it "fails on only sign" $ do
      runParser parseInt "+" `shouldBe` Left (GenericError "Expected unsigned integer")
      runParser parseInt "-" `shouldBe` Left (GenericError "Expected unsigned integer")
    it "fails on sign with non-digit" $ do
      runParser parseInt "+abc" `shouldBe` Left (GenericError "Expected unsigned integer")
      runParser parseInt "-abc" `shouldBe` Left (GenericError "Expected unsigned integer")

  describe "parseRight/parseLeft (failures)" $ do
    it "fails if left parser fails (parseRight)" $ do
      runParser (parseRight (parseChar 'a') (parseChar 'b')) "xbc"
        `shouldBe` Left (PError (ExpectedChar 'a' Nothing))
    it "fails if right parser fails (parseRight)" $ do
      runParser (parseRight (parseChar 'a') (parseChar 'b')) "axc"
        `shouldBe` Left (PError (ExpectedChar 'b' Nothing))
    it "fails if left parser fails (parseLeft)" $ do
      runParser (parseLeft (parseChar 'a') (parseChar 'b')) "xbc"
        `shouldBe` Left (PError (ExpectedChar 'a' Nothing))
    it "fails if right parser fails (parseLeft)" $ do
      runParser (parseLeft (parseChar 'a') (parseChar 'b')) "axc"
        `shouldBe` Left (PError (ExpectedChar 'b' Nothing))

  describe "withError (failures)" $ do
    it "overrides any error" $ do
      runParser (withError (parseChar 'a') (GenericError "fail")) "" `shouldBe` Left (GenericError "fail")

  describe "withContext (all cases)" $ do
    it "adds context to RError" $ do
      withContext "CTX" (RError (RuntimeError "fail" Nothing))
        `shouldBe` RError (RuntimeError "CTX: fail" Nothing)
    it "adds context to PError InvalidSyntax" $ do
      withContext "CTX" (PError (InvalidSyntax "fail" Nothing))
        `shouldBe` PError (InvalidSyntax "CTX: fail" Nothing)
    it "does not change other errors" $ do
      withContext "CTX" (PError (UnexpectedChar 'x' Nothing))
        `shouldBe` PError (UnexpectedChar 'x' Nothing)
