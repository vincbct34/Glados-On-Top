{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Spec
-}

module Main (main) where

import Test.Hspec
import Parser
import Types
import Eval
import Builtins
import Env

main :: IO ()
main = hspec $ do
    parserTests
    evalTests
    builtinTests
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
    
    describe "parseString" $ do
        it "parses simple strings" $ do
            runParser parseString "\"hello\"" `shouldBe` Right (String "hello", "")
        it "parses empty strings" $ do
            runParser parseString "\"\"" `shouldBe` Right (String "", "")
    
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
    
    describe "eval define" $ do
        it "defines new variables" $ do
            let expr = List [Atom "define", Atom "x", Number 42]
            let result = eval expr emptyEnv
            case result of
                Right (Number 42, newEnv) -> lookupVar "x" newEnv `shouldBe` Just (Number 42)
                _ -> expectationFailure "define should succeed"
    
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
                Right (Function (UserFunction ["x"] (Atom "x")), _) -> return ()
                _ -> expectationFailure "lambda should create user function"

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

-- Integration Tests
integrationTests :: Spec
integrationTests = describe "Integration Tests" $ do
    describe "Complex expressions" $ do
        it "evaluates nested arithmetic" $ do
            let expr = List [Atom "+", List [Atom "*", Number 2, Number 3], Number 4]
            eval expr builtinEnv `shouldBe` Right (Number 10, builtinEnv)
        
        it "evaluates factorial function" $ do
            let factDef = List [Atom "define", Atom "fact", 
                               List [Atom "lambda", List [Atom "n"],
                                    List [Atom "if", List [Atom "=", Atom "n", Number 0],
                                         Number 1,
                                         List [Atom "*", Atom "n", 
                                              List [Atom "fact", List [Atom "-", Atom "n", Number 1]]]]]]
            let factCall = List [Atom "fact", Number 5]
            case eval factDef builtinEnv of
                Right (_, env') -> eval factCall env' `shouldBe` Right (Number 120, env')
                Left err -> expectationFailure $ "factorial definition failed: " ++ err
        
        it "evaluates nested lists" $ do
            let expr = List [Atom "car", List [Atom "cdr", List [Atom "list", Number 1, Number 2, Number 3]]]
            eval expr builtinEnv `shouldBe` Right (Number 2, builtinEnv)

-- Helper functions
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
