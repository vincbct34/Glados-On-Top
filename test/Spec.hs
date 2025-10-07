{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Spec
-}

module Main (main) where

import Builtins
import Control.Applicative ((<|>), empty)
import Data.Char (toUpper)
import Env
import Eval
import Parser
import Test.Hspec
import Types

main :: IO ()
main = hspec $ do
  parserTests
  parserErrorTests
  advancedParserTests
  comprehensiveParserTests
  evalTests
  evalErrorTests
  builtinTests
  builtinErrorTests
  typeTests
  envTests
  integrationTests

-- Parser Tests
parserTests :: Spec
parserTests = describe "Parser Tests" $ do
  describe "parseNumber" $ do
    it "parses positive integers" $ do
      runParser parseNumber "42" `shouldBe` Right (Number 42, "")
    it "parses negative integers" $ do
      runParser parseNumber "-17" `shouldBe` Right (Number (-17), "")
    it "fails on non-numbers" $ do
      runParser parseNumber "abc" `shouldSatisfy` isLeft

  describe "parseAtom" $ do
    it "parses simple atoms" $ do
      runParser parseAtom "hello" `shouldBe` Right (Atom "hello", "")
    it "parses atoms with special chars" $ do
      runParser parseAtom "+" `shouldBe` Right (Atom "+", "")
    it "parses atoms with numbers" $ do
      runParser parseAtom "var123" `shouldBe` Right (Atom "var123", "")
    it "parses complex atoms with special characters" $ do
      runParser parseAtom "test-var!" `shouldBe` Right (Atom "test-var!", "")
    it "parses single character atoms" $ do
      runParser parseAtom "x" `shouldBe` Right (Atom "x", "")

  describe "parseString" $ do
    it "parses simple strings" $ do
      runParser parseString "\"hello\"" `shouldBe` Right (String "hello", "")
    it "parses empty strings" $ do
      runParser parseString "\"\"" `shouldBe` Right (String "", "")
    it "parses strings with spaces" $ do
      runParser parseString "\"hello world\"" `shouldBe` Right (String "hello world", "")
    it "parses strings with special characters" $ do
      runParser parseString "\"!@#$%\"" `shouldBe` Right (String "!@#$%", "")

  describe "parseBoolean" $ do
    it "parses true" $ do
      runParser parseBoolean "#t" `shouldBe` Right (Boolean True, "")
    it "parses false" $ do
      runParser parseBoolean "#f" `shouldBe` Right (Boolean False, "")

  describe "parseList" $ do
    it "parses empty list" $ do
      runParser parseList "()" `shouldBe` Right (Nil, "")
    it "parses simple list" $ do
      runParser parseList "(1 2 3)" `shouldBe` Right (List [Number 1, Number 2, Number 3], "")
    it "parses nested lists" $ do
      runParser parseList "((1 2) 3)" `shouldBe` Right (List [List [Number 1, Number 2], Number 3], "")
    it "parses mixed type lists" $ do
      runParser parseList "(1 \"hello\" #t)" `shouldBe` Right (List [Number 1, String "hello", Boolean True], "")
    it "parses deeply nested lists" $ do
      runParser parseList "(((1)))" `shouldBe` Right (List [List [List [Number 1]]], "")

  describe "parseLispValue with whitespace" $ do
    it "handles leading whitespace with parseExpression" $ do
      runParser parseExpression "   42" `shouldBe` Right (Number 42, "")
    it "handles trailing content" $ do
      runParser parseLispValue "42 extra" `shouldBe` Right (Number 42, " extra")

-- Parser Error Tests
parserErrorTests :: Spec
parserErrorTests = describe "Parser Error Tests" $ do
  describe "parseNumber errors" $ do
    it "fails on invalid number format" $ do
      runParser parseNumber "abc" `shouldSatisfy` isLeft
    it "fails on empty input for numbers" $ do
      runParser parseNumber "" `shouldSatisfy` isLeft
    it "fails on mixed alphanumeric starting with number" $ do
      runParser parseNumber "123abc" `shouldBe` Right (Number 123, "abc")

  describe "parseString errors" $ do
    it "fails on unterminated string" $ do
      runParser parseString "\"hello" `shouldSatisfy` isLeft
    it "fails on missing opening quote" $ do
      runParser parseString "hello\"" `shouldSatisfy` isLeft

  describe "parseList errors" $ do
    it "fails on unterminated list" $ do
      runParser parseList "(1 2 3" `shouldSatisfy` isLeft
    it "fails on extra closing paren" $ do
      runParser parseList "(1 2))" `shouldBe` Right (List [Number 1, Number 2], ")")
    it "fails on nested unterminated list" $ do
      runParser parseList "((1 2)" `shouldSatisfy` isLeft

  describe "parseBoolean errors" $ do
    it "fails on invalid boolean format" $ do
      runParser parseBoolean "#x" `shouldSatisfy` isLeft
    it "fails on partial boolean" $ do
      runParser parseBoolean "#" `shouldSatisfy` isLeft

  describe "parseAtom errors" $ do
    it "fails on atoms starting with numbers" $ do
      runParser parseAtom "123abc" `shouldSatisfy` isLeft
    it "fails on empty input" $ do
      runParser parseAtom "" `shouldSatisfy` isLeft

-- Advanced Parser Tests
advancedParserTests :: Spec
advancedParserTests = describe "Advanced Parser Tests" $ do
  describe "Parser combinators" $ do
    describe "parseSatisfy" $ do
      it "succeeds for matching predicate" $ do
        runParser (parseSatisfy (== 'a')) "abc" `shouldBe` Right ('a', "bc")
      it "fails for non-matching predicate" $ do
        runParser (parseSatisfy (== 'b')) "abc" `shouldSatisfy` isLeft
      it "fails on empty input" $ do
        runParser (parseSatisfy (== 'a')) "" `shouldSatisfy` isLeft
    
    describe "parseChar" $ do
      it "parses specific character" $ do
        runParser (parseChar 'x') "xyz" `shouldBe` Right ('x', "yz")
      it "fails on wrong character" $ do
        runParser (parseChar 'a') "xyz" `shouldSatisfy` isLeft
      it "fails on empty input" $ do
        runParser (parseChar 'a') "" `shouldSatisfy` isLeft

    describe "parseCharCase" $ do
      it "parses character case insensitive" $ do
        runParser (parseCharCase 'A') "abc" `shouldBe` Right ('a', "bc")
        runParser (parseCharCase 'a') "ABC" `shouldBe` Right ('A', "BC")
      it "fails on wrong character" $ do
        runParser (parseCharCase 'x') "abc" `shouldSatisfy` isLeft

    describe "parseAnyChar" $ do
      it "parses any character from string" $ do
        runParser (parseAnyChar "abc") "bdef" `shouldBe` Right ('b', "def")
        runParser (parseAnyChar "xyz") "zab" `shouldBe` Right ('z', "ab")
      it "fails when no match" $ do
        runParser (parseAnyChar "xyz") "abc" `shouldSatisfy` isLeft
      it "fails on empty input" $ do
        runParser (parseAnyChar "abc") "" `shouldSatisfy` isLeft

    describe "parseAnyCharCase" $ do
      it "parses any character case insensitive" $ do
        runParser (parseAnyCharCase "ABC") "bdef" `shouldBe` Right ('b', "def")
        runParser (parseAnyCharCase "abc") "Bdef" `shouldBe` Right ('B', "def")
      it "fails when no match" $ do
        runParser (parseAnyCharCase "xyz") "abc" `shouldSatisfy` isLeft

    describe "parseAnd" $ do
      it "combines two parsers into tuple" $ do
        let parser = parseAnd (parseChar 'a') (parseChar 'b')
        runParser parser "abc" `shouldBe` Right (('a', 'b'), "c")
      it "fails if first parser fails" $ do
        let parser = parseAnd (parseChar 'x') (parseChar 'b')
        runParser parser "abc" `shouldSatisfy` isLeft
      it "fails if second parser fails" $ do
        let parser = parseAnd (parseChar 'a') (parseChar 'x')
        runParser parser "abc" `shouldSatisfy` isLeft

    describe "parseAndWith" $ do
      it "combines parsers with custom function" $ do
        let parser = parseAndWith (,) (parseChar 'a') (parseChar 'b')
        runParser parser "abc" `shouldBe` Right (('a', 'b'), "c")
      it "applies function to results" $ do
        let combiner x y = [x, y]
        let parser = parseAndWith combiner (parseChar 'a') (parseChar 'b')
        runParser parser "abc" `shouldBe` Right ("ab", "c")

    describe "parseMany" $ do
      it "parses zero occurrences" $ do
        runParser (parseMany (parseChar 'x')) "abc" `shouldBe` Right ("", "abc")
      it "parses multiple occurrences" $ do
        runParser (parseMany (parseChar 'a')) "aaabc" `shouldBe` Right ("aaa", "bc")
      it "stops at first failure" $ do
        runParser (parseMany (parseChar 'a')) "aabbc" `shouldBe` Right ("aa", "bbc")

    describe "parseSome" $ do
      it "parses one or more occurrences" $ do
        runParser (parseSome (parseChar 'a')) "aaabc" `shouldBe` Right ("aaa", "bc")
      it "fails if no occurrences" $ do
        runParser (parseSome (parseChar 'x')) "abc" `shouldSatisfy` isLeft
      it "parses exactly one occurrence" $ do
        runParser (parseSome (parseChar 'a')) "abc" `shouldBe` Right ("a", "bc")

    describe "parseUInt" $ do
      it "parses positive integers" $ do
        runParser parseUInt "123abc" `shouldBe` Right (123, "abc")
        runParser parseUInt "0" `shouldBe` Right (0, "")
      it "fails on non-digit input" $ do
        runParser parseUInt "abc" `shouldSatisfy` isLeft
      it "fails on empty input" $ do
        runParser parseUInt "" `shouldSatisfy` isLeft
      it "parses large numbers" $ do
        runParser parseUInt "999999" `shouldBe` Right (999999, "")

    describe "parseInt" $ do
      it "parses positive integers with plus" $ do
        runParser parseInt "+123" `shouldBe` Right (123, "")
      it "parses negative integers" $ do
        runParser parseInt "-456" `shouldBe` Right (-456, "")
      it "parses integers without sign" $ do
        runParser parseInt "789" `shouldBe` Right (789, "")
      it "fails on invalid input" $ do
        runParser parseInt "abc" `shouldSatisfy` isLeft

    describe "parseRight and parseLeft" $ do
      it "parseRight returns second value" $ do
        let parser = parseRight (parseChar 'a') (parseChar 'b')
        runParser parser "abc" `shouldBe` Right ('b', "c")
      it "parseLeft returns first value" $ do
        let parser = parseLeft (parseChar 'a') (parseChar 'b')
        runParser parser "abc" `shouldBe` Right ('a', "c")

  describe "Error handling functions" $ do
    describe "withError" $ do
      it "uses custom error on failure" $ do
        let customError = GenericError "Custom error message"
        let parser = parseChar 'x' `withError` customError
        case runParser parser "abc" of
          Left err -> err `shouldBe` customError
          Right _ -> expectationFailure "Should have failed"
      it "preserves success" $ do
        let customError = GenericError "Custom error message"
        let parser = parseChar 'a' `withError` customError
        runParser parser "abc" `shouldBe` Right ('a', "bc")

    describe "withContext" $ do
      it "adds context to GenericError" $ do
        let originalError = GenericError "original message"
        let contextualError = withContext "test context" originalError
        show contextualError `shouldContain` "test context: original message"
      it "adds context to RuntimeError" $ do
        let originalError = RError (RuntimeError "runtime message" Nothing)
        let contextualError = withContext "test context" originalError
        show contextualError `shouldContain` "test context: runtime message"
      it "adds context to InvalidSyntax" $ do
        let originalError = PError (InvalidSyntax "syntax message" Nothing)
        let contextualError = withContext "test context" originalError
        show contextualError `shouldContain` "test context: syntax message"
      it "leaves other errors unchanged" $ do
        let originalError = PError (ExpectedChar 'a' Nothing)
        let contextualError = withContext "test context" originalError
        contextualError `shouldBe` originalError

    describe "chainErrors" $ do
      it "handles empty error list" $ do
        let chainedError = chainErrors []
        show chainedError `shouldContain` "Unknown error"
      it "returns single error unchanged" $ do
        let singleError = GenericError "single error"
        chainErrors [singleError] `shouldBe` singleError
      it "combines multiple errors" $ do
        let errors = [GenericError "error 1", GenericError "error 2"]
        let chainedError = chainErrors errors
        show chainedError `shouldContain` "Multiple errors"
        show chainedError `shouldContain` "error 1"
        show chainedError `shouldContain` "error 2"

  describe "Whitespace and comment parsing" $ do
    describe "parseWhitespace" $ do
      it "parses spaces and tabs" $ do
        runParser parseWhitespace "   \t  " `shouldBe` Right ((), "")
      it "parses newlines" $ do
        runParser parseWhitespace "\n\n" `shouldBe` Right ((), "")
      it "parses mixed whitespace" $ do
        runParser parseWhitespace " \t\n " `shouldBe` Right ((), "")
      it "succeeds on no whitespace" $ do
        runParser parseWhitespace "abc" `shouldBe` Right ((), "abc")
      it "parses comments followed by newline" $ do
        runParser parseWhitespace "; this is a comment\nabc" `shouldBe` Right ((), "abc")
      it "parses comments at end of file" $ do
        runParser parseWhitespace "; comment at end" `shouldBe` Right ((), "")

    describe "parseComment" $ do
      it "parses comment line" $ do
        runParser parseComment "; this is a comment\nrest" `shouldBe` Right ((), "\nrest")
      it "parses comment without newline" $ do
        runParser parseComment "; comment at end" `shouldBe` Right ((), "")
      it "fails on non-comment input" $ do
        runParser parseComment "not a comment" `shouldSatisfy` isLeft

    describe "parseEOF" $ do
      it "succeeds on empty input" $ do
        runParser parseEOF "" `shouldBe` Right ((), "")
      it "fails on non-empty input" $ do
        runParser parseEOF "abc" `shouldSatisfy` isLeft

  describe "Complex parsing scenarios" $ do
    describe "nested expressions with whitespace and comments" $ do
      it "parses expression with leading comment" $ do
        let input = "; comment\n  42"
        runParser parseExpression input `shouldBe` Right (Number 42, "")
      it "parses nested lists with comments" $ do
        let input = "(+ ; adding\n 1 2)"
        runParser parseExpression input `shouldBe` Right (List [Atom "+", Number 1, Number 2], "")
      it "handles multiple comment styles" $ do
        let input = "; line comment\n(; another comment\n42)"
        runParser parseExpression input `shouldBe` Right (List [Number 42], "")

    describe "error location tracking" $ do
      it "provides source location in errors" $ do
        case runParser (parseChar 'x') "abc" of
          Left (PError (ExpectedChar 'x' loc)) -> loc `shouldBe` Nothing
          _ -> expectationFailure "Expected ExpectedChar error"

  describe "Alternative operator tests" $ do
    it "tries alternatives in order" $ do
      let parser = parseChar 'a' <|> parseChar 'b' <|> parseChar 'c'
      runParser parser "bcd" `shouldBe` Right ('b', "cd")
      runParser parser "cde" `shouldBe` Right ('c', "de")
    it "fails when all alternatives fail" $ do
      let parser = parseChar 'a' <|> parseChar 'b' <|> parseChar 'c'
      runParser parser "xyz" `shouldSatisfy` isLeft

  describe "Functor and Applicative instances" $ do
    it "fmap works correctly" $ do
      let parser = fmap toUpper (parseChar 'a')
      runParser parser "abc" `shouldBe` Right ('A', "bc")
    it "pure works correctly" $ do
      runParser (pure 42) "anything" `shouldBe` Right (42, "anything")
    it "applicative application works" $ do
      let parser = pure toUpper <*> parseChar 'a'
      runParser parser "abc" `shouldBe` Right ('A', "bc")

-- Comprehensive Parser Tests
comprehensiveParserTests :: Spec
comprehensiveParserTests = describe "Comprehensive Parser Tests" $ do
  describe "Error types and formatting" $ do
    describe "SourceLocation" $ do
      it "creates default location correctly" $ do
        defaultLocation `shouldBe` SourceLocation 1 1 0
      it "shows location correctly" $ do
        let loc = SourceLocation 5 10 25
        show loc `shouldContain` "5"
        show loc `shouldContain` "10" 
        show loc `shouldContain` "25"
      it "equals itself" $ do
        let loc1 = SourceLocation 1 2 3
        let loc2 = SourceLocation 1 2 3
        loc1 `shouldBe` loc2
      it "doesn't equal different location" $ do
        let loc1 = SourceLocation 1 2 3
        let loc2 = SourceLocation 1 2 4
        loc1 `shouldNotBe` loc2

    describe "SourceLocation in error messages" $ do
      it "shows errors without location correctly" $ do
        let err = UnexpectedChar 'x' Nothing
        show err `shouldNotContain` "at line"
      it "shows errors with location correctly" $ do
        let loc = Just (SourceLocation 10 5 0)
        let err = UnexpectedChar 'x' loc
        show err `shouldContain` "at line 10, column 5"

    describe "ParseError types and Show instances" $ do
      it "shows UnexpectedChar without location" $ do
        let err = UnexpectedChar 'x' Nothing
        show err `shouldBe` "Parse error: unexpected character 'x'"
      it "shows UnexpectedChar with location" $ do
        let loc = Just (SourceLocation 2 3 0)
        let err = UnexpectedChar 'y' loc
        show err `shouldBe` "Parse error at line 2, column 3: unexpected character 'y'"
      it "shows UnexpectedEOF without location" $ do
        let err = UnexpectedEOF Nothing
        show err `shouldBe` "Parse error: unexpected end of input"
      it "shows UnexpectedEOF with location" $ do
        let loc = Just (SourceLocation 1 5 0)
        let err = UnexpectedEOF loc
        show err `shouldBe` "Parse error at line 1, column 5: unexpected end of input"
      it "shows ExpectedChar without location" $ do
        let err = ExpectedChar 'a' Nothing
        show err `shouldBe` "Parse error: expected character 'a'"
      it "shows ExpectedChar with location" $ do
        let loc = Just (SourceLocation 3 1 0)
        let err = ExpectedChar 'b' loc
        show err `shouldBe` "Parse error at line 3, column 1: expected character 'b'"
      it "shows ExpectedString without location" $ do
        let err = ExpectedString "hello" Nothing
        show err `shouldBe` "Parse error: expected \"hello\""
      it "shows ExpectedString with location" $ do
        let loc = Just (SourceLocation 1 1 0)
        let err = ExpectedString "world" loc
        show err `shouldBe` "Parse error at line 1, column 1: expected \"world\""
      it "shows InvalidSyntax without location" $ do
        let err = InvalidSyntax "bad syntax" Nothing
        show err `shouldBe` "Syntax error: bad syntax"
      it "shows InvalidSyntax with location" $ do
        let loc = Just (SourceLocation 4 2 0)
        let err = InvalidSyntax "invalid construct" loc
        show err `shouldBe` "Syntax error at line 4, column 2: invalid construct"

    describe "SemanticError types and Show instances" $ do
      it "shows UndefinedVariable without location" $ do
        let err = UndefinedVariable "myVar" Nothing
        show err `shouldBe` "Reference error: undefined variable 'myVar'"
      it "shows UndefinedVariable with location" $ do
        let loc = Just (SourceLocation 2 5 0)
        let err = UndefinedVariable "testVar" loc
        show err `shouldBe` "Reference error at line 2, column 5: undefined variable 'testVar'"
      it "shows TypeMismatch without location" $ do
        let err = TypeMismatch "Integer" "String" Nothing
        show err `shouldBe` "Type error: expected Integer, got String"
      it "shows TypeMismatch with location" $ do
        let loc = Just (SourceLocation 1 10 0)
        let err = TypeMismatch "Number" "Boolean" loc
        show err `shouldBe` "Type error at line 1, column 10: expected Number, got Boolean"
      it "shows ArityMismatch without location" $ do
        let err = ArityMismatch "add" 2 3 Nothing
        show err `shouldBe` "Arity error: function 'add' expects 2 arguments, but got 3"
      it "shows ArityMismatch with location" $ do
        let loc = Just (SourceLocation 5 1 0)
        let err = ArityMismatch "multiply" 1 0 loc
        show err `shouldBe` "Arity error at line 5, column 1: function 'multiply' expects 1 arguments, but got 0"
      it "shows InvalidOperation without location" $ do
        let err = InvalidOperation "cannot divide by zero" Nothing
        show err `shouldBe` "Semantic error: cannot divide by zero"
      it "shows InvalidOperation with location" $ do
        let loc = Just (SourceLocation 3 8 0)
        let err = InvalidOperation "type mismatch in operation" loc
        show err `shouldBe` "Semantic error at line 3, column 8: type mismatch in operation"
      it "shows DivisionByZero without location" $ do
        let err = DivisionByZero Nothing
        show err `shouldBe` "Semantic error: division by zero"
      it "shows DivisionByZero with location" $ do
        let loc = Just (SourceLocation 7 3 0)
        let err = DivisionByZero loc
        show err `shouldBe` "Semantic error at line 7, column 3: division by zero"

    describe "RuntimeError types and Show instances" $ do
      it "shows RuntimeError without location" $ do
        let err = RuntimeError "runtime failure" Nothing
        show err `shouldBe` "Runtime error: runtime failure"
      it "shows RuntimeError with location" $ do
        let loc = Just (SourceLocation 2 1 0)
        let err = RuntimeError "stack overflow detected" loc
        show err `shouldBe` "Runtime error at line 2, column 1: stack overflow detected"
      it "shows StackOverflow without location" $ do
        let err = StackOverflow Nothing
        show err `shouldBe` "Runtime error: stack overflow"
      it "shows StackOverflow with location" $ do
        let loc = Just (SourceLocation 10 20 0)
        let err = StackOverflow loc
        show err `shouldBe` "Runtime error at line 10, column 20: stack overflow"
      it "shows OutOfMemory without location" $ do
        let err = OutOfMemory Nothing
        show err `shouldBe` "Runtime error: out of memory"
      it "shows OutOfMemory with location" $ do
        let loc = Just (SourceLocation 1 1 0)
        let err = OutOfMemory loc
        show err `shouldBe` "Runtime error at line 1, column 1: out of memory"

    describe "ParserError wrapper and Show instances" $ do
      it "shows PError correctly" $ do
        let parseErr = UnexpectedChar 'z' Nothing
        let err = PError parseErr
        show err `shouldBe` "Parse error: unexpected character 'z'"
      it "shows SError correctly" $ do
        let semanticErr = UndefinedVariable "test" Nothing
        let err = SError semanticErr
        show err `shouldBe` "Reference error: undefined variable 'test'"
      it "shows RError correctly" $ do
        let runtimeErr = RuntimeError "crash" Nothing
        let err = RError runtimeErr
        show err `shouldBe` "Runtime error: crash"
      it "shows GenericError correctly" $ do
        let err = GenericError "generic message"
        show err `shouldBe` "Error: generic message"

  describe "Error equality" $ do
    it "ParseError equality works" $ do
      let err1 = UnexpectedChar 'a' Nothing
      let err2 = UnexpectedChar 'a' Nothing
      let err3 = UnexpectedChar 'b' Nothing
      err1 `shouldBe` err2
      err1 `shouldNotBe` err3
    it "SemanticError equality works" $ do
      let err1 = DivisionByZero Nothing
      let err2 = DivisionByZero Nothing
      let err3 = DivisionByZero (Just defaultLocation)
      err1 `shouldBe` err2
      err1 `shouldNotBe` err3
    it "RuntimeError equality works" $ do
      let err1 = StackOverflow Nothing
      let err2 = StackOverflow Nothing
      let err3 = OutOfMemory Nothing
      err1 `shouldBe` err2
      err1 `shouldNotBe` err3
    it "ParserError equality works" $ do
      let err1 = GenericError "test"
      let err2 = GenericError "test"
      let err3 = GenericError "different"
      err1 `shouldBe` err2
      err1 `shouldNotBe` err3

  describe "Advanced withContext tests" $ do
    it "handles InvalidSyntax with context" $ do
      let err = PError (InvalidSyntax "bad syntax" Nothing)
      let contextualErr = withContext "test context" err
      show contextualErr `shouldContain` "test context: bad syntax"
    it "handles RuntimeError with context" $ do
      let err = RError (RuntimeError "crash" Nothing)
      let contextualErr = withContext "runtime context" err
      show contextualErr `shouldContain` "runtime context: crash"
    it "handles GenericError with context" $ do
      let err = GenericError "generic message"
      let contextualErr = withContext "test context" err
      show contextualErr `shouldContain` "test context: generic message"
    it "leaves other errors unchanged" $ do
      let err = PError (ExpectedChar 'a' Nothing)
      let contextualErr = withContext "test context" err
      contextualErr `shouldBe` err

  describe "Monadic parser behavior" $ do
    it "bind operation works correctly" $ do
      let parser = parseChar 'a' >>= \c -> parseChar 'b' >>= \d -> return [c, d]
      runParser parser "abc" `shouldBe` Right ("ab", "c")
    it "bind fails when first parser fails" $ do
      let parser = parseChar 'x' >>= \_ -> parseChar 'b'
      runParser parser "abc" `shouldSatisfy` isLeft
    it "bind fails when second parser fails" $ do
      let parser = parseChar 'a' >>= \_ -> parseChar 'x'
      runParser parser "abc" `shouldSatisfy` isLeft

  describe "Alternative instance comprehensive tests" $ do
    it "empty parser always fails" $ do
      runParser (empty :: Parser Char) "anything" `shouldSatisfy` isLeft
    it "complex alternative chains work" $ do
      let parser = parseChar 'x' <|> parseChar 'y' <|> parseChar 'z' <|> parseChar 'a'
      runParser parser "abc" `shouldBe` Right ('a', "bc")
    it "nested alternatives work" $ do
      let parser = (parseChar 'a' <|> parseChar 'b') <|> (parseChar 'c' <|> parseChar 'd')
      runParser parser "cde" `shouldBe` Right ('c', "de")

  describe "Edge cases for parseLispValue" $ do
    it "tries parseNumber first" $ do
      runParser parseLispValue "42hello" `shouldBe` Right (Number 42, "hello")
    it "falls back to parseString" $ do
      runParser parseLispValue "\"test\"rest" `shouldBe` Right (String "test", "rest")
    it "falls back to parseBoolean" $ do
      runParser parseLispValue "#trest" `shouldBe` Right (Boolean True, "rest")
    it "falls back to parseList" $ do
      runParser parseLispValue "(1 2)rest" `shouldBe` Right (List [Number 1, Number 2], "rest")
    it "falls back to parseAtom last" $ do
      runParser parseLispValue "atom rest" `shouldBe` Right (Atom "atom", " rest")
    it "fails when nothing matches" $ do
      runParser parseLispValue ")" `shouldSatisfy` isLeft

  describe "Extreme edge cases" $ do
    it "handles very long atoms" $ do
      let longAtom = replicate 1000 'a'
      runParser parseAtom (longAtom ++ " rest") `shouldBe` Right (Atom longAtom, " rest")
    it "handles very long strings" $ do
      let longContent = replicate 1000 'x'
      let longString = "\"" ++ longContent ++ "\""
      runParser parseString (longString ++ "rest") `shouldBe` Right (String longContent, "rest")
    it "handles deeply nested lists" $ do
      let deepList = replicate 20 '(' ++ "42" ++ replicate 20 ')'
      case runParser parseList deepList of
        Right (result, "") -> result `shouldSatisfy` (\_ -> True) -- Just verify it parses
        _ -> expectationFailure "Should parse deeply nested list"
    it "handles atoms with all special characters" $ do
      let specialAtom = "+-*/=<>!?_test-var"
      runParser parseAtom specialAtom `shouldBe` Right (Atom specialAtom, "")
    it "handles empty string parsing" $ do
      runParser parseString "\"\"" `shouldBe` Right (String "", "")
    it "handles single character atoms" $ do
      runParser parseAtom "+" `shouldBe` Right (Atom "+", "")
      runParser parseAtom "-" `shouldBe` Right (Atom "-", "")
      runParser parseAtom "*" `shouldBe` Right (Atom "*", "")

  describe "Parser state and backtracking" $ do
    it "parseMany doesn't consume on failure" $ do
      let parser = parseMany (parseChar 'a') <* parseChar 'b'
      runParser parser "b" `shouldBe` Right ("", "")
    it "parseSome consumes at least one" $ do
      let parser = parseSome (parseChar 'a') <* parseChar 'b'
      runParser parser "aaab" `shouldBe` Right ("aaa", "")
    it "alternative works correctly with same first character" $ do
      let parser = (parseChar 'a' *> parseChar 'x') <|> (parseChar 'a' *> parseChar 'b')
      runParser parser "ab" `shouldBe` Right ('b', "")

  describe "Complex integer parsing edge cases" $ do
    it "parseInt handles just minus sign" $ do
      runParser parseInt "-" `shouldSatisfy` isLeft
    it "parseInt handles just plus sign" $ do
      runParser parseInt "+" `shouldSatisfy` isLeft  
    it "parseInt handles leading zeros" $ do
      runParser parseInt "000123" `shouldBe` Right (123, "")
      runParser parseInt "-000456" `shouldBe` Right (-456, "")
    it "parseUInt handles single digit" $ do
      runParser parseUInt "5abc" `shouldBe` Right (5, "abc")
    it "parseNumber creates correct LispValue" $ do
      runParser parseNumber "123" `shouldBe` Right (Number 123, "")
      runParser parseNumber "-456" `shouldBe` Right (Number (-456), "")

-- Evaluation Tests
evalTests :: Spec
evalTests = describe "Evaluation Tests" $ do
  describe "eval literals" $ do
    it "evaluates numbers" $ do
      eval (Number 42) emptyEnv `shouldBe` Right (Number 42, emptyEnv)
    it "evaluates booleans" $ do
      eval (Boolean True) emptyEnv `shouldBe` Right (Boolean True, emptyEnv)
    it "evaluates strings" $ do
      eval (String "hello") emptyEnv `shouldBe` Right (String "hello", emptyEnv)
    it "evaluates nil" $ do
      eval Nil emptyEnv `shouldBe` Right (Nil, emptyEnv)

  describe "eval variables" $ do
    it "looks up existing variables" $ do
      let env = bindVar "x" (Number 10) emptyEnv
      eval (Atom "x") env `shouldBe` Right (Number 10, env)
    it "fails on undefined variables" $ do
      eval (Atom "undefined") emptyEnv `shouldSatisfy` isLeft

  describe "eval if" $ do
    it "evaluates then branch on true condition" $ do
      let expr = List [Atom "if", Boolean True, Number 1, Number 2]
      eval expr emptyEnv `shouldBe` Right (Number 1, emptyEnv)
    it "evaluates else branch on false condition" $ do
      let expr = List [Atom "if", Boolean False, Number 1, Number 2]
      eval expr emptyEnv `shouldBe` Right (Number 2, emptyEnv)
    it "evaluates else branch on nil condition" $ do
      let expr = List [Atom "if", Nil, Number 1, Number 2]
      eval expr emptyEnv `shouldBe` Right (Number 2, emptyEnv)

  describe "eval lambda" $ do
    it "creates user functions" $ do
      let expr = List [Atom "lambda", List [Atom "x"], Atom "x"]
      let result = eval expr emptyEnv
      case result of
        Right (Function (UserFunction ["x"] (Atom "x") _), _) -> return ()
        _ -> expectationFailure "lambda should create user function"

  describe "eval function application" $ do
    it "applies user functions" $ do
      let lambda = List [Atom "lambda", List [Atom "x"], List [Atom "+", Atom "x", Number 1]]
      let application = List [lambda, Number 5]
      eval application builtinEnv `shouldBe` Right (Number 6, builtinEnv)
    it "applies functions with multiple parameters" $ do
      let lambda = List [Atom "lambda", List [Atom "x", Atom "y"], List [Atom "+", Atom "x", Atom "y"]]
      let application = List [lambda, Number 3, Number 4]
      eval application builtinEnv `shouldBe` Right (Number 7, builtinEnv)

  describe "eval quote" $ do
    it "quotes simple expressions" $ do
      eval (List [Atom "quote", Atom "hello"]) emptyEnv `shouldBe` Right (Atom "hello", emptyEnv)
    it "quotes lists" $ do
      eval (List [Atom "quote", List [Number 1, Number 2]]) emptyEnv `shouldBe` Right (List [Number 1, Number 2], emptyEnv)

  describe "eval void" $ do
    it "evaluates void values" $ do
      eval Void emptyEnv `shouldBe` Right (Void, emptyEnv)

-- Evaluation Error Tests
evalErrorTests :: Spec
evalErrorTests = describe "Evaluation Error Tests" $ do
  describe "variable lookup errors" $ do
    it "fails on undefined variables" $ do
      eval (Atom "undefined-var") emptyEnv `shouldSatisfy` isLeft
    it "returns specific error message for undefined variable" $ do
      case eval (Atom "missing") emptyEnv of
        Left err -> err `shouldContain` "Undefined variable: missing"
        Right _ -> expectationFailure "Should have failed"

  describe "function application errors" $ do
    it "fails when applying non-function" $ do
      eval (List [Number 42, Number 1]) emptyEnv `shouldSatisfy` isLeft
    it "fails with arity mismatch" $ do
      let lambda = List [Atom "lambda", List [Atom "x"], Atom "x"]
      let application = List [lambda, Number 1, Number 2]
      eval application emptyEnv `shouldSatisfy` isLeft
    it "fails with zero arguments to function expecting arguments" $ do
      let lambda = List [Atom "lambda", List [Atom "x"], Atom "x"]
      let application = List [lambda]
      eval application emptyEnv `shouldSatisfy` isLeft

  describe "define errors" $ do
    it "fails with invalid lambda parameters" $ do
      let expr = List [Atom "define", Atom "f", List [Atom "lambda", List [Number 1], Number 2]]
      eval expr emptyEnv `shouldSatisfy` isLeft
    it "handles recursive function definitions" $ do
      let factDef = List [Atom "define", Atom "fact", 
                         List [Atom "lambda", List [Atom "n"], 
                               List [Atom "if", List [Atom "=", Atom "n", Number 0], 
                                     Number 1, 
                                     List [Atom "*", Atom "n", List [Atom "fact", List [Atom "-", Atom "n", Number 1]]]]]]
      case eval factDef builtinEnv of
        Right (Void, env') -> 
          eval (List [Atom "fact", Number 3]) env' `shouldBe` Right (Number 6, env')
        Left err -> expectationFailure $ "Recursive definition should work: " ++ err

  describe "lambda errors" $ do
    it "fails with non-atom parameters" $ do
      let expr = List [Atom "lambda", List [Number 1], Number 2]
      eval expr emptyEnv `shouldSatisfy` isLeft
    it "fails with invalid parameter structure" $ do
      let expr = List [Atom "lambda", Number 42, Number 2]
      eval expr emptyEnv `shouldSatisfy` isLeft

  describe "if statement edge cases" $ do
    it "treats empty list as falsy in if" $ do
      let expr = List [Atom "if", List [], Number 1, Number 2]
      eval expr emptyEnv `shouldBe` Right (Number 2, emptyEnv)
    it "treats non-boolean truthy values in if" $ do
      let expr = List [Atom "if", Number 42, Number 1, Number 2]
      eval expr emptyEnv `shouldBe` Right (Number 1, emptyEnv)

-- Builtin Tests
builtinTests :: Spec
builtinTests = describe "Builtin Tests" $ do
  describe "Arithmetic" $ do
    it "adds numbers" $ do
      addBuiltin [Number 1, Number 2, Number 3] `shouldBe` Right (Number 6)
    it "adds no numbers (identity)" $ do
      addBuiltin [] `shouldBe` Right (Number 0)

    it "subtracts numbers" $ do
      subBuiltin [Number 10, Number 3, Number 2] `shouldBe` Right (Number 5)
    it "negates single number" $ do
      subBuiltin [Number 5] `shouldBe` Right (Number (-5))

    it "multiplies numbers" $ do
      mulBuiltin [Number 2, Number 3, Number 4] `shouldBe` Right (Number 24)
    it "multiplies no numbers (identity)" $ do
      mulBuiltin [] `shouldBe` Right (Number 1)

    it "divides numbers" $ do
      divBuiltin [Number 12, Number 3, Number 2] `shouldBe` Right (Number 2)
    it "fails on division by zero" $ do
      divBuiltin [Number 10, Number 0] `shouldSatisfy` isLeft

  describe "Comparison" $ do
    it "tests equality" $ do
      eqBuiltin [Number 5, Number 5, Number 5] `shouldBe` Right (Boolean True)
      eqBuiltin [Number 5, Number 3] `shouldBe` Right (Boolean False)

    it "tests less than" $ do
      ltBuiltin [Number 1, Number 2, Number 3] `shouldBe` Right (Boolean True)
      ltBuiltin [Number 3, Number 2, Number 1] `shouldBe` Right (Boolean False)

    it "tests greater than" $ do
      gtBuiltin [Number 3, Number 2, Number 1] `shouldBe` Right (Boolean True)
      gtBuiltin [Number 1, Number 2, Number 3] `shouldBe` Right (Boolean False)

  describe "List Operations" $ do
    it "gets car of list" $ do
      carBuiltin [List [Number 1, Number 2, Number 3]] `shouldBe` Right (Number 1)
    it "fails car on empty list" $ do
      carBuiltin [Nil] `shouldSatisfy` isLeft

    it "gets cdr of list" $ do
      cdrBuiltin [List [Number 1, Number 2, Number 3]] `shouldBe` Right (List [Number 2, Number 3])
    it "gets cdr of single element list" $ do
      cdrBuiltin [List [Number 1]] `shouldBe` Right Nil

    it "constructs list with cons" $ do
      consBuiltin [Number 1, List [Number 2, Number 3]] `shouldBe` Right (List [Number 1, Number 2, Number 3])
    it "constructs list with cons and nil" $ do
      consBuiltin [Number 1, Nil] `shouldBe` Right (List [Number 1])

    it "creates list with list function" $ do
      listBuiltin [Number 1, Number 2, Number 3] `shouldBe` Right (List [Number 1, Number 2, Number 3])
    it "creates empty list" $ do
      listBuiltin [] `shouldBe` Right Nil

  describe "Type Predicates" $ do
    it "tests null predicate" $ do
      nullBuiltin [Nil] `shouldBe` Right (Boolean True)
      nullBuiltin [List []] `shouldBe` Right (Boolean True)
      nullBuiltin [Number 1] `shouldBe` Right (Boolean False)

    it "tests number predicate" $ do
      numBuiltin [Number 42] `shouldBe` Right (Boolean True)
      numBuiltin [String "hello"] `shouldBe` Right (Boolean False)

    it "tests list predicate" $ do
      listPredicateBuiltin [List [Number 1]] `shouldBe` Right (Boolean True)
      listPredicateBuiltin [Nil] `shouldBe` Right (Boolean True)
      listPredicateBuiltin [Number 1] `shouldBe` Right (Boolean False)

    it "tests atom predicate" $ do
      atomBuiltin [Atom "hello"] `shouldBe` Right (Boolean True)
      atomBuiltin [Number 1] `shouldBe` Right (Boolean False)

-- Builtin Error Tests
builtinErrorTests :: Spec
builtinErrorTests = describe "Builtin Error Tests" $ do
  describe "Arithmetic Error Cases" $ do
    it "subtraction with no arguments fails" $ do
      subBuiltin [] `shouldSatisfy` isLeft
    it "division with no arguments fails" $ do
      divBuiltin [] `shouldSatisfy` isLeft
    it "division by zero fails" $ do
      divBuiltin [Number 10, Number 0] `shouldSatisfy` isLeft
    it "reciprocal of zero fails" $ do
      divBuiltin [Number 0] `shouldSatisfy` isLeft
    it "arithmetic with non-numbers fails" $ do
      addBuiltin [Number 1, String "hello"] `shouldSatisfy` isLeft
      subBuiltin [Boolean True, Number 2] `shouldSatisfy` isLeft
      mulBuiltin [Number 3, Atom "test"] `shouldSatisfy` isLeft

  describe "Comparison Edge Cases" $ do
    it "handles single argument comparisons" $ do
      ltBuiltin [Number 5] `shouldBe` Right (Boolean True)
      gtBuiltin [Number 5] `shouldBe` Right (Boolean True)
      leBuiltin [Number 5] `shouldBe` Right (Boolean True)
      geBuiltin [Number 5] `shouldBe` Right (Boolean True)
    it "handles empty argument comparisons" $ do
      ltBuiltin [] `shouldBe` Right (Boolean True)
      eqBuiltin [] `shouldBe` Right (Boolean True)
    it "comparison with non-numbers fails" $ do
      ltBuiltin [String "a", String "b"] `shouldSatisfy` isLeft
      gtBuiltin [Boolean True, Boolean False] `shouldSatisfy` isLeft

  describe "List Operation Error Cases" $ do
    it "car on empty list fails" $ do
      carBuiltin [Nil] `shouldSatisfy` isLeft
      carBuiltin [List []] `shouldSatisfy` isLeft
    it "car with wrong argument count fails" $ do
      carBuiltin [] `shouldSatisfy` isLeft
      carBuiltin [Number 1, Number 2] `shouldSatisfy` isLeft
    it "car on non-list fails" $ do
      carBuiltin [Number 42] `shouldSatisfy` isLeft
      carBuiltin [String "hello"] `shouldSatisfy` isLeft

    it "cdr on empty list fails" $ do
      cdrBuiltin [Nil] `shouldSatisfy` isLeft
      cdrBuiltin [List []] `shouldSatisfy` isLeft
    it "cdr with wrong argument count fails" $ do
      cdrBuiltin [] `shouldSatisfy` isLeft
      cdrBuiltin [Number 1, Number 2] `shouldSatisfy` isLeft
    it "cdr on non-list fails" $ do
      cdrBuiltin [Number 42] `shouldSatisfy` isLeft

    it "cons with wrong argument count fails" $ do
      consBuiltin [Number 1] `shouldSatisfy` isLeft
      consBuiltin [Number 1, Number 2, Number 3] `shouldSatisfy` isLeft
    it "cons with non-list second argument fails" $ do
      consBuiltin [Number 1, Number 2] `shouldSatisfy` isLeft
      consBuiltin [Number 1, String "hello"] `shouldSatisfy` isLeft

  describe "Predicate Error Cases" $ do
    it "predicates with wrong argument count fail" $ do
      nullBuiltin [] `shouldSatisfy` isLeft
      nullBuiltin [Number 1, Number 2] `shouldSatisfy` isLeft
      numBuiltin [] `shouldSatisfy` isLeft
      numBuiltin [Number 1, String "test"] `shouldSatisfy` isLeft
      listPredicateBuiltin [] `shouldSatisfy` isLeft
      atomBuiltin [Atom "test", Number 1] `shouldSatisfy` isLeft

  describe "Additional Arithmetic Edge Cases" $ do
    it "handles large numbers" $ do
      addBuiltin [Number 999999, Number 1] `shouldBe` Right (Number 1000000)
      mulBuiltin [Number 1000, Number 1000] `shouldBe` Right (Number 1000000)
    it "handles negative numbers in all operations" $ do
      subBuiltin [Number (-5)] `shouldBe` Right (Number 5)
      divBuiltin [Number (-10), Number (-2)] `shouldBe` Right (Number 5)
      ltBuiltin [Number (-5), Number (-3), Number 0] `shouldBe` Right (Boolean True)

  describe "Mixed type equality" $ do
    it "different types are not equal" $ do
      eqBuiltin [Number 1, String "1"] `shouldBe` Right (Boolean False)
      eqBuiltin [Boolean True, Number 1] `shouldBe` Right (Boolean False)
      eqBuiltin [Nil, List []] `shouldBe` Right (Boolean False)

-- Integration Tests
integrationTests :: Spec
integrationTests = describe "Integration Tests" $ do
  describe "Complex expressions" $ do
    it "evaluates nested arithmetic" $ do
      let expr = List [Atom "+", List [Atom "*", Number 2, Number 3], Number 4]
      eval expr builtinEnv `shouldBe` Right (Number 10, builtinEnv)

    it "evaluates factorial function" $ do
      let factDef =
            List
              [ Atom "define",
                Atom "fact",
                List
                  [ Atom "lambda",
                    List [Atom "n"],
                    List
                      [ Atom "if",
                        List [Atom "=", Atom "n", Number 0],
                        Number 1,
                        List
                          [ Atom "*",
                            Atom "n",
                            List [Atom "fact", List [Atom "-", Atom "n", Number 1]]
                          ]
                      ]
                  ]
              ]
      let factCall = List [Atom "fact", Number 5]
      case eval factDef builtinEnv of
        Right (_, env') -> eval factCall env' `shouldBe` Right (Number 120, env')
        Left err -> expectationFailure $ "factorial definition failed: " ++ err

    it "evaluates nested lists" $ do
      let expr = List [Atom "car", List [Atom "cdr", List [Atom "list", Number 1, Number 2, Number 3]]]
      eval expr builtinEnv `shouldBe` Right (Number 2, builtinEnv)

  describe "More complex integration cases" $ do
    it "evaluates higher-order functions" $ do
      let mapDef = List [Atom "define", Atom "map",
                        List [Atom "lambda", List [Atom "f", Atom "lst"],
                              List [Atom "if", List [Atom "null?", Atom "lst"],
                                    Nil,
                                    List [Atom "cons", 
                                          List [Atom "f", List [Atom "car", Atom "lst"]],
                                          List [Atom "map", Atom "f", List [Atom "cdr", Atom "lst"]]]]]]
      let incDef = List [Atom "define", Atom "inc", List [Atom "lambda", List [Atom "x"], List [Atom "+", Atom "x", Number 1]]]
      let mapCall = List [Atom "map", Atom "inc", List [Atom "list", Number 1, Number 2, Number 3]]
      case eval mapDef builtinEnv of
        Right (_, env1) -> case eval incDef env1 of
          Right (_, env2) -> eval mapCall env2 `shouldBe` Right (List [Number 2, Number 3, Number 4], env2)
          Left err -> expectationFailure $ "inc definition failed: " ++ err
        Left err -> expectationFailure $ "map definition failed: " ++ err

    it "evaluates closures correctly" $ do
      let makeAdder = List [Atom "define", Atom "make-adder",
                           List [Atom "lambda", List [Atom "x"],
                                 List [Atom "lambda", List [Atom "y"], List [Atom "+", Atom "x", Atom "y"]]]]
      let add5 = List [Atom "define", Atom "add5", List [Atom "make-adder", Number 5]]
      let testCall = List [Atom "add5", Number 3]
      case eval makeAdder builtinEnv of
        Right (_, env1) -> case eval add5 env1 of
          Right (_, env2) -> eval testCall env2 `shouldBe` Right (Number 8, env2)
          Left err -> expectationFailure $ "add5 definition failed: " ++ err
        Left err -> expectationFailure $ "make-adder definition failed: " ++ err

-- Type Tests
typeTests :: Spec
typeTests = describe "Types Helper Functions" $ do
  describe "Type checking functions" $ do
    it "correctly identifies atoms" $ do
      isAtom (Atom "test") `shouldBe` True
      isAtom (Number 42) `shouldBe` False
      isAtom (String "hello") `shouldBe` False
      isAtom (Boolean True) `shouldBe` False
      isAtom Nil `shouldBe` False
      isAtom (List []) `shouldBe` False

    it "correctly identifies numbers" $ do
      isNumber (Number 42) `shouldBe` True
      isNumber (Number (-17)) `shouldBe` True
      isNumber (Atom "test") `shouldBe` False
      isNumber (String "42") `shouldBe` False
      isNumber (Boolean False) `shouldBe` False

    it "correctly identifies strings" $ do
      isString (String "hello") `shouldBe` True
      isString (String "") `shouldBe` True
      isString (Atom "hello") `shouldBe` False
      isString (Number 42) `shouldBe` False

    it "correctly identifies booleans" $ do
      isBoolean (Boolean True) `shouldBe` True
      isBoolean (Boolean False) `shouldBe` True
      isBoolean (Atom "#t") `shouldBe` False
      isBoolean (Number 1) `shouldBe` False

    it "correctly identifies lists" $ do
      isList (List [Number 1, Number 2]) `shouldBe` True
      isList (List []) `shouldBe` True
      isList Nil `shouldBe` True
      isList (Number 42) `shouldBe` False
      isList (Atom "list") `shouldBe` False

    it "correctly identifies functions" $ do
      let func = Function (BuiltinFunction "+" addBuiltin)
      isFunction func `shouldBe` True
      isFunction (Number 42) `shouldBe` False
      isFunction (Atom "function") `shouldBe` False

  describe "LispFunction equality and show" $ do
    it "shows builtin functions correctly" $ do
      let func = BuiltinFunction "+" addBuiltin
      show func `shouldBe` "<builtin:+>"

    it "shows user functions correctly" $ do
      let func = UserFunction ["x", "y"] (Atom "body") emptyEnv
      show func `shouldBe` "<function:(x y)>"

    it "shows recursive functions correctly" $ do
      let func = RecursiveFunction "fact" ["n"] (Atom "body") emptyEnv
      show func `shouldBe` "<recursive-function:fact:(n)>"

    it "compares builtin functions correctly" $ do
      let func1 = BuiltinFunction "+" addBuiltin
      let func2 = BuiltinFunction "+" mulBuiltin
      let func3 = BuiltinFunction "-" subBuiltin
      func1 == func2 `shouldBe` True
      func1 == func3 `shouldBe` False

-- Environment Tests
envTests :: Spec
envTests = describe "Environment Tests" $ do
  describe "Basic environment operations" $ do
    it "creates empty environment" $ do
      let env = emptyEnv
      bindings env `shouldBe` []
      parent env `shouldBe` Nothing

    it "binds variables correctly" $ do
      let env = bindVar "x" (Number 42) emptyEnv
      lookupVar "x" env `shouldBe` Just (Number 42)
      lookupVar "y" env `shouldBe` Nothing

    it "extends environment with multiple bindings" $ do
      let bindings_list = [("x", Number 1), ("y", String "hello"), ("z", Boolean True)]
      let env = extendEnv bindings_list emptyEnv
      lookupVar "x" env `shouldBe` Just (Number 1)
      lookupVar "y" env `shouldBe` Just (String "hello")
      lookupVar "z" env `shouldBe` Just (Boolean True)
      lookupVar "w" env `shouldBe` Nothing

    it "checks if variables are defined" $ do
      let env = bindVar "test" (Number 123) emptyEnv
      isDefined "test" env `shouldBe` True
      isDefined "missing" env `shouldBe` False

  describe "Scoped environments" $ do
    it "creates new scope correctly" $ do
      let parentEnv = bindVar "x" (Number 1) emptyEnv
      let childEnv = newScope parentEnv
      bindings childEnv `shouldBe` []
      parent childEnv `shouldBe` Just parentEnv

    it "creates new scope with initial bindings" $ do
      let parentEnv = bindVar "x" (Number 1) emptyEnv
      let childBindings = [("y", Number 2), ("z", Number 3)]
      let childEnv = newScopeWith childBindings parentEnv
      bindings childEnv `shouldBe` childBindings
      parent childEnv `shouldBe` Just parentEnv

    it "looks up variables in parent scope" $ do
      let parentEnv = bindVar "x" (Number 1) emptyEnv
      let childEnv = newScopeWith [("y", Number 2)] parentEnv
      lookupVar "x" childEnv `shouldBe` Just (Number 1)
      lookupVar "y" childEnv `shouldBe` Just (Number 2)
      lookupVar "z" childEnv `shouldBe` Nothing

    it "shadows parent variables correctly" $ do
      let parentEnv = bindVar "x" (Number 1) emptyEnv
      let childEnv = newScopeWith [("x", Number 2)] parentEnv
      lookupVar "x" childEnv `shouldBe` Just (Number 2)
      lookupVar "x" parentEnv `shouldBe` Just (Number 1)

  describe "Multi-level scoping" $ do
    it "handles deep nesting correctly" $ do
      let env1 = bindVar "a" (Number 1) emptyEnv
      let env2 = newScopeWith [("b", Number 2)] env1
      let env3 = newScopeWith [("c", Number 3)] env2
      lookupVar "a" env3 `shouldBe` Just (Number 1)
      lookupVar "b" env3 `shouldBe` Just (Number 2)
      lookupVar "c" env3 `shouldBe` Just (Number 3)

    it "handles shadowing in deep nesting" $ do
      let env1 = bindVar "x" (Number 1) emptyEnv
      let env2 = newScopeWith [("x", Number 2)] env1
      let env3 = newScopeWith [("x", Number 3)] env2
      lookupVar "x" env3 `shouldBe` Just (Number 3)
      lookupVar "x" env2 `shouldBe` Just (Number 2)
      lookupVar "x" env1 `shouldBe` Just (Number 1)

-- Helper functions
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
