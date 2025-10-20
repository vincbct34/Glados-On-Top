{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Main entry point - Parse and compile .rat files
-}

module Main (main) where

import qualified Data.Text.IO as TIO
import Ratatouille.Bytecode (compileProgram)
import Ratatouille.Parser.Proc (pProgram)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: Glados-On-Top-exe <file.rat>"
      putStrLn "       Parse and compile a Ratatouille source file to bytecode"
      exitFailure
    (filePath : _) -> do
      processFile filePath

processFile :: FilePath -> IO ()
processFile filePath = do
  putStrLn $ "Reading file: " ++ filePath
  content <- TIO.readFile filePath
  
  putStrLn "\n=== Parsing Phase ==="
  case parse pProgram filePath content of
    Left err -> do
      putStrLn "Parse Error:"
      putStrLn $ errorBundlePretty err
      exitFailure
    Right ast -> do
      putStrLn "[OK] Parsing successful!"
      putStrLn "\nAST:"
      print ast
      
      putStrLn "\n=== Compilation Phase ==="
      let bytecode = compileProgram ast
      putStrLn "[OK] Compilation successful!"
      putStrLn "\nBytecode:"
      mapM_ print bytecode
      
      putStrLn "\n=== Summary ==="
      putStrLn $ "Total instructions: " ++ show (length bytecode)
      exitSuccess
