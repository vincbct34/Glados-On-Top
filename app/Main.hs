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

import qualified Ratatouille.Error.Format as ErrFmt
import Ratatouille.Error.Types (makeRichError, RichError)

-- Use color helpers from ErrFmt
colorize :: String -> String -> String
colorize = ErrFmt.colorize

formatRichError :: RichError -> String
formatRichError = ErrFmt.formatRichError

putStrLnColored :: String -> String -> IO ()
putStrLnColored c txt = putStrLn (colorize c txt)

processFile :: FilePath -> IO ()
processFile filePath = do
  putStrLnColored ("\ESC[1m\ESC[36m") ("Reading file: " ++ filePath)
  content <- TIO.readFile filePath

  putStrLn ""
  putStrLnColored ("\ESC[1m\ESC[34m") "PARSING PHASE"
  putStrLn ""
  case parse pProgram filePath content of
    Left err -> do
      putStrLnColored ("\ESC[1m\ESC[31m") "PARSE ERROR DETECTED"
      putStrLn ""
      let errMsgStr = errorBundlePretty err
          rich = makeRichError filePath errMsgStr content
      putStrLn $ formatRichError rich
      putStrLn ""
      exitFailure
    Right ast -> do
      putStrLnColored ("\ESC[1m\ESC[32m") "Parsing successful"
      putStrLn ""
      print ast

      putStrLn ""
      putStrLnColored ("\ESC[1m\ESC[34m") "COMPILATION PHASE"
      let bytecode = compileProgram ast
      putStrLnColored ("\ESC[1m\ESC[32m") "Compilation successful"
      mapM_ print bytecode
      exitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "No input file provided!"
      exitFailure
    (filePath : _) -> do
      processFile filePath
