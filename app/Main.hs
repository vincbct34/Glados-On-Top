{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Main
-}

module Main (main) where

import Builtins
import Control.Monad (unless, void, when)
import Eval
import Parser
import System.Environment (getArgs)
import System.IO (hFlush, hIsTerminalDevice, isEOF, stdin, stdout)
import Types

main :: IO ()
main = do
  args <- getArgs
  isTerminal <- hIsTerminalDevice stdin
  case args of
    [] ->
      if isTerminal
        then repl builtinEnv True
        else getContents >>= processInput
    ["-h"] -> putStrLn "Usage: glados [filename]"
    [filename] -> runFile filename
    _ -> putStrLn "Usage: glados [filename]"

processInput :: String -> IO ()
processInput input =
  case parseMultipleExpressions input of
    Left parseErr ->
      putStrLn $ "Parse Error: " ++ show parseErr
    Right expressions ->
      void (evaluateExpressions expressions builtinEnv)

runFile :: String -> IO ()
runFile filename = do
  input <- readFile filename
  case parseMultipleExpressions input of
    Left parseErr ->
      putStrLn $ "Parse Error: " ++ show parseErr
    Right expressions ->
      void (evaluateExpressions expressions builtinEnv)

repl :: Env -> Bool -> IO ()
repl env showPrompt = do
  when showPrompt $
    putStr "> " >> hFlush stdout
  eof <- isEOF
  unless eof $ do
    line <- getLine
    unless (all (`elem` " \t\n") line) $ do
      (newEnv, continue) <- processLine line env showPrompt
      when continue $
        repl newEnv showPrompt

processLine :: String -> Env -> Bool -> IO (Env, Bool)
processLine line env _showPrompt =
  case parseMultipleExpressions line of
    Left parseErr ->
      putStrLn ("Parse Error: " ++ show parseErr) >> return (env, True)
    Right expressions ->
      case expressions of
        (Atom "exit" : _) -> return (env, False)
        _ -> do
          (_, newEnv) <- evaluateExpressionsRepl expressions env
          return (newEnv, True)

evaluateExpressionsRepl :: [LispValue] -> Env -> IO (Maybe LispValue, Env)
evaluateExpressionsRepl [] env = return (Nothing, env)
evaluateExpressionsRepl [expr] env =
  case eval expr env of
    Left evalErr ->
      putStrLn ("Eval Error: " ++ evalErr) >> return (Nothing, env)
    Right (result, newEnv) ->
      case result of
        Nil -> return (Just result, newEnv)
        Void -> return (Just result, newEnv)
        _ -> putStrLn (showResult result) >> return (Just result, newEnv)
evaluateExpressionsRepl (expr : exprs) env =
  case eval expr env of
    Left evalErr ->
      putStrLn ("Eval Error: " ++ evalErr) >> evaluateExpressionsRepl exprs env
    Right (result, newEnv) ->
      case result of
        Nil -> evaluateExpressionsRepl exprs newEnv
        Void -> evaluateExpressionsRepl exprs newEnv
        _ -> putStrLn (showResult result) >> evaluateExpressionsRepl exprs newEnv

parseMultipleExpressions :: String -> Either ParserError [LispValue]
parseMultipleExpressions = parseExpressions

parseExpressions :: String -> Either ParserError [LispValue]
parseExpressions input =
  case runParser parseExpressionsParser input of
    Left err -> Left err
    Right (exprs, _) -> Right exprs
  where
    parseExpressionsParser = do
      parseWhitespace
      exprs <- parseMany (parseWhitespace *> parseLispValue <* parseWhitespace)
      parseWhitespace
      parseEOF
      return exprs

evaluateExpressions :: [LispValue] -> Env -> IO (Maybe LispValue, Env)
evaluateExpressions [] env = return (Nothing, env)
evaluateExpressions [expr] env =
  case eval expr env of
    Left evalErr ->
      putStrLn ("Eval Error: " ++ evalErr) >> return (Nothing, env)
    Right (result, newEnv) ->
      case result of
        Nil -> return (Just result, newEnv)
        Void -> return (Just result, newEnv)
        _ -> putStrLn (showResult result) >> return (Just result, newEnv)
evaluateExpressions (expr : exprs) env =
  case eval expr env of
    Left evalErr ->
      putStrLn ("Eval Error: " ++ evalErr) >> return (Nothing, env)
    Right (result, newEnv) ->
      case result of
        Nil -> evaluateExpressions exprs newEnv
        Void -> evaluateExpressions exprs newEnv
        _ -> putStrLn (showResult result) >> evaluateExpressions exprs newEnv
