{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Integration tests based on real examples
-}

module IntegrationSpec (spec) where

import Data.Text (Text, pack)
import Ratatouille.AST
import Ratatouille.Bytecode.Compiler (compileProgram)
import Ratatouille.Bytecode.Types
import Ratatouille.Parser.Proc (pProgram)
import Test.Hspec
import Text.Megaparsec (errorBundlePretty, parse)

-- Helper to parse a program
parseProgram :: Text -> Either String Program
parseProgram input =
  case parse pProgram "" input of
    Left err -> Left $ errorBundlePretty err
    Right prog -> Right prog

-- Helper to compile a program
compileTestProgram :: Text -> Either String [Instruction]
compileTestProgram input =
  case parseProgram input of
    Left err -> Left err
    Right prog -> Right $ compileProgram prog

spec :: Spec
spec = do
  describe "Integration Tests - Parsing and Compilation" $ do
    simpleAdditionTests
    factorialTests
    conditionalTests
    arrayTests
    tupleTests
    actorModelTests
    importTests

simpleAdditionTests :: Spec
simpleAdditionTests = describe "Simple Addition" $ do
  it "parses simple addition program" $ do
    let source =
          pack $
            unlines
              [ "proc add(a, b) {",
                "  a + b",
                "}",
                "",
                "proc main() {",
                "  add(2, 3)",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2

  it "compiles simple addition program" $ do
    let source =
          pack $
            unlines
              [ "proc add(a, b) {",
                "  a + b",
                "}",
                "",
                "proc main() {",
                "  add(2, 3)",
                "}"
              ]
    case compileTestProgram source of
      Left err -> expectationFailure $ "Failed to compile: " ++ err
      Right bytecode -> length bytecode `shouldSatisfy` (> 0)

factorialTests :: Spec
factorialTests = describe "Factorial" $ do
  it "parses recursive factorial" $ do
    let source =
          pack $
            unlines
              [ "proc factorial(n) {",
                "  if n <= 1 then",
                "    1",
                "  else",
                "    n * factorial(n - 1)",
                "}",
                "",
                "proc main() {",
                "  factorial(5)",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2

  it "compiles recursive factorial" $ do
    let source =
          pack $
            unlines
              [ "proc factorial(n) {",
                "  if n <= 1 then",
                "    1",
                "  else",
                "    n * factorial(n - 1)",
                "}",
                "",
                "proc main() {",
                "  factorial(5)",
                "}"
              ]
    case compileTestProgram source of
      Left err -> expectationFailure $ "Failed to compile: " ++ err
      Right bytecode -> length bytecode `shouldSatisfy` (> 0)

conditionalTests :: Spec
conditionalTests = describe "Conditionals" $ do
  it "parses if-then-else expressions" $ do
    let source =
          pack $
            unlines
              [ "proc test(x) {",
                "  if x > 0 then",
                "    \"positive\"",
                "  else",
                "    \"negative\"",
                "}",
                "",
                "proc main() {",
                "  test(5)",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2

  it "parses nested conditionals" $ do
    let source =
          pack $
            unlines
              [ "proc classify(x) {",
                "  if x > 0 then",
                "    if x > 10 then",
                "      \"big\"",
                "    else",
                "      \"small\"",
                "  else",
                "    \"negative\"",
                "}",
                "",
                "proc main() {",
                "  classify(5)",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2

arrayTests :: Spec
arrayTests = describe "Arrays" $ do
  it "parses array literals" $ do
    let source =
          pack $
            unlines
              [ "proc main() {",
                "  [1, 2, 3, 4, 5]",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 1

  it "parses array indexing" $ do
    let source =
          pack $
            unlines
              [ "proc main() {",
                "  let arr = [1, 2, 3]",
                "  arr[0]",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 1

tupleTests :: Spec
tupleTests = describe "Tuples" $ do
  it "parses tuple literals" $ do
    let source =
          pack $
            unlines
              [ "proc main() {",
                "  (1, \"hello\", true)",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 1

  it "parses tuple destructuring" $ do
    let source =
          pack $
            unlines
              [ "proc main() {",
                "  let (a, b) = (1, 2)",
                "  a + b",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 1

actorModelTests :: Spec
actorModelTests = describe "Actor Model" $ do
  it "parses spawn expression" $ do
    let source =
          pack $
            unlines
              [ "proc Worker() {",
                "  receive {",
                "    | x -> x",
                "  }",
                "}",
                "",
                "proc main() {",
                "  spawn Worker()",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2

  it "parses message sending" $ do
    let source =
          pack $
            unlines
              [ "proc Worker() {",
                "  receive {",
                "    | x -> x",
                "  }",
                "}",
                "",
                "proc main() {",
                "  let w = spawn Worker()",
                "  w <- 42",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2

  it "parses process with state" $ do
    let source =
          pack $
            unlines
              [ "proc Counter(initial) {",
                "  state: initial,",
                "  receive {",
                "    | :increment -> state = state + 1",
                "    | :get -> state",
                "  }",
                "}",
                "",
                "proc main() {",
                "  spawn Counter(0)",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2

importTests :: Spec
importTests = describe "Import System" $ do
  it "parses import all" $ do
    let source =
          pack $
            unlines
              [ "import \"utils.rat\"",
                "",
                "proc main() {",
                "  42",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2

  it "parses import single" $ do
    let source =
          pack $
            unlines
              [ "import Counter from \"utils.rat\"",
                "",
                "proc main() {",
                "  42",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2

  it "parses import selected" $ do
    let source =
          pack $
            unlines
              [ "import {Counter, Timer} from \"utils.rat\"",
                "",
                "proc main() {",
                "  42",
                "}"
              ]
    case parseProgram source of
      Left err -> expectationFailure $ "Failed to parse: " ++ err
      Right (Program defs) -> length defs `shouldBe` 2
