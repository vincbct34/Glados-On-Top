{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- ParserSpec
-}

module ParserSpec (spec) where

import Data.Text (Text, pack)
import Ratatouille.AST
import Ratatouille.Parser
import Test.Hspec
import Text.Megaparsec (errorBundlePretty, parse)

-- | Helper function to test parsing results.
shouldParseAs :: Text -> Expr -> Expectation
shouldParseAs input expected =
  case parse pProgram "" input of
    Left err -> expectationFailure $ "Parsing failed:\n" ++ errorBundlePretty err
    Right result -> result `shouldBe` expected

-- | Helper function to test parsing failures.
shouldFail :: (Text -> Either String Expr) -> Text -> Expectation
shouldFail parser input =
  case parser input of
    Left _ -> return ()
    Right result -> expectationFailure $ "Expected failure but got: " ++ show result

spec :: Spec
spec = describe "Ratatouille Parser" $ do
  describe "Expressions (pProgram)" $ do
    it "parses a positive integer expression" $
      shouldParseAs (pack "123") (ELiteral (LInt 123))

    it "parses a negative integer expression" $
      shouldParseAs (pack "-45") (ELiteral (LInt (-45)))

    it "parses a string literal expression" $
      shouldParseAs (pack "\"Hello Ratatouille!\"") (ELiteral (LString (pack "Hello Ratatouille!")))

    it "parses an atom expression" $
      shouldParseAs (pack ":myAtom") (EAtom (pack "myAtom"))

    it "handles whitespace around literals" $
      shouldParseAs (pack "  123  ") (ELiteral (LInt 123))

    it "handles whitespace around atoms" $
      shouldParseAs (pack " :another_atom ") (EAtom (pack "another_atom"))

    it "parses a variable identifier" $
      shouldParseAs (pack "my_variable") (EVar (pack "my_variable"))

    it "fails to parse an identifier starting with a number" $
      shouldFail
        ( \input -> case parse pProgram "" input of
            Left err -> Left (errorBundlePretty err)
            Right result -> Right result
        )
        (pack "1variable")

    it "parses an empty tuple" $
      shouldParseAs (pack "{}") (ETuple [])

    it "parses a single-element tuple" $
      shouldParseAs (pack "{ 123 }") (ETuple [ELiteral (LInt 123)])

    it "parses a multi-element tuple with mixed types" $
      shouldParseAs
        (pack "{ :deposit, 50, my_account }")
        (ETuple [EAtom (pack "deposit"), ELiteral (LInt 50), EVar (pack "my_account")])

    it "parses a tuple with trailing comma" $
      shouldParseAs (pack "{ 1, 2, }") (ETuple [ELiteral (LInt 1), ELiteral (LInt 2)])

    it "parses a function call with arguments" $
      shouldParseAs
        (pack "{ :my_function, :arg1, 123, \"hello\" }")
        ( ETuple
            [ EAtom (pack "my_function"),
              EAtom (pack "arg1"),
              ELiteral (LInt 123),
              ELiteral (LString (pack "hello"))
            ]
        )

    it "parses a nested function call" $
      shouldParseAs
        (pack "{ :outer_func, { :inner_func, 10 }, 20 }")
        ( ETuple
            [ EAtom (pack "outer_func"),
              ETuple [EAtom (pack "inner_func"), ELiteral (LInt 10)],
              ELiteral (LInt 20)
            ]
        )

    it "parses a function call with variable as function name" $
      shouldParseAs
        (pack "{ my_func_var, 1, 2 }")
        ( ETuple
            [ EVar (pack "my_func_var"),
              ELiteral (LInt 1),
              ELiteral (LInt 2)
            ]
        )

    it "parses simple addition" $
      shouldParseAs (pack "1 + 2") (EBinOp Add (ELiteral (LInt 1)) (ELiteral (LInt 2)))

    it "parses simple multiplication" $
      shouldParseAs (pack "3 * 4") (EBinOp Mul (ELiteral (LInt 3)) (ELiteral (LInt 4)))

    it "respects operator precedence (multiplication before addition)" $
      shouldParseAs
        (pack "1 + 2 * 3")
        ( EBinOp
            Add
            (ELiteral (LInt 1))
            (EBinOp Mul (ELiteral (LInt 2)) (ELiteral (LInt 3)))
        )

    it "handles left associativity (subtraction)" $
      shouldParseAs
        (pack "10 - 5 - 2")
        ( EBinOp
            Sub
            (EBinOp Sub (ELiteral (LInt 10)) (ELiteral (LInt 5)))
            (ELiteral (LInt 2))
        )

    it "handles parentheses for custom precedence" $
      shouldParseAs
        (pack "(1 + 2) * 3")
        ( EBinOp
            Mul
            (EBinOp Add (ELiteral (LInt 1)) (ELiteral (LInt 2))) -- <<< CORRECTION HERE
            (ELiteral (LInt 3))
        )

    it "parses complex expression with mixed operators" $
      shouldParseAs
        (pack "10 / 2 + 5 * 3 - 1")
        ( EBinOp
            Sub
            ( EBinOp
                Add
                (EBinOp Div (ELiteral (LInt 10)) (ELiteral (LInt 2)))
                (EBinOp Mul (ELiteral (LInt 5)) (ELiteral (LInt 3)))
            )
            (ELiteral (LInt 1))
        )
