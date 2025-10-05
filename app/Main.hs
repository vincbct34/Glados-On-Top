{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Main
-}

module Main (main) where

import Builtins
import Control.Monad (void)
import Eval
import Parser
import Types

main :: IO ()
main = do
  input <- getContents
  case parseMultipleExpressions input of
    Left parseErr ->
      putStrLn $ "Parse Error: " ++ show parseErr
    Right expressions ->
      void (evaluateExpressions expressions builtinEnv)

-- Parse multiple expressions from input
parseMultipleExpressions :: String -> Either ParserError [LispValue]
parseMultipleExpressions = parseExpressions

-- Parse a sequence of LISP expressions with proper whitespace and comment handling
parseExpressions :: String -> Either ParserError [LispValue]
parseExpressions input =
  case runParser parseExpressionsParser input of
    Left err -> Left err
    Right (exprs, _) -> Right exprs
  where
    parseExpressionsParser = do
      parseWhitespace -- Skip initial whitespace and comments
      exprs <- parseMany (parseWhitespace *> parseLispValue <* parseWhitespace)
      parseWhitespace -- Skip final whitespace and comments
      parseEOF
      return exprs

-- Evaluate multiple expressions in sequence, showing each result
evaluateExpressions :: [LispValue] -> Env -> IO (Maybe LispValue, Env)
evaluateExpressions [] env = return (Nothing, env)
evaluateExpressions [expr] env =
  case eval expr env of
    Left evalErr ->
      putStrLn ("Eval Error: " ++ evalErr) >> return (Nothing, env)
    Right (result, newEnv) ->
      putStrLn (showResult result) >> return (Just result, newEnv)
evaluateExpressions (expr : exprs) env =
  case eval expr env of
    Left evalErr ->
      putStrLn ("Eval Error: " ++ evalErr) >> return (Nothing, env)
    Right (result, newEnv) ->
      ( case result of
          Function _ -> return ()
          _ -> putStrLn $ showResult result
      )
        >> evaluateExpressions exprs newEnv
