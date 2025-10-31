{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Main entry point - Parse and compile .rat files
-}

module Main (main) where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Ratatouille.AST (Definition(..), ImportDecl(..), ImportItems(..), Program(..), ProcDefinition(..))
import Ratatouille.Bytecode.Compiler (compileProgram)
import Ratatouille.Bytecode.Encoder (writeBinaryFile)
import Ratatouille.Parser.Proc (pProgram)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (replaceExtension, takeDirectory, (</>))
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: Glados-On-Top-exe <file.rat> [-o output.rtbc]"
      putStrLn "       Parse and compile a Ratatouille source file to binary bytecode"
      putStrLn ""
      putStrLn "Options:"
      putStrLn "  -o <file>    Specify output bytecode file (default: <input>.rtbc)"
      exitFailure
    _ -> do
      let (inputFile, outputFile) = parseArgs args
      processFile inputFile outputFile

parseArgs :: [String] -> (FilePath, FilePath)
parseArgs args =
  case args of
    [input] -> (input, replaceExtension input ".rtbc")
    [input, "-o", output] -> (input, output)
    (input : "-o" : output : _) -> (input, output)
    (input : _) -> (input, replaceExtension input ".rtbc")
    _ -> error "Invalid arguments"

processFile :: FilePath -> FilePath -> IO ()
processFile inputPath outputPath = do
  -- Load and resolve all imports recursively
  result <- loadProgramWithImports inputPath Set.empty
  
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ err
      exitFailure
    Right mergedProgram -> do
      -- Compile the merged program
      let bytecode = compileProgram mergedProgram
      
      writeBinaryFile outputPath bytecode
      
      putStrLn $ "Compiled successfully: " ++ inputPath ++ " -> " ++ outputPath
      exitSuccess

-- Load a program and recursively resolve all imports
-- Returns a merged Program with aimport {Greeter} from "../modules/utils.rat"
loadProgramWithImports :: FilePath -> Set.Set FilePath -> IO (Either String Program)
loadProgramWithImports filePath visited
  | filePath `Set.member` visited = 
      return $ Left $ "Circular import detected: " ++ filePath
  | otherwise = do
      let newVisited = Set.insert filePath visited
      
      -- Check if file exists
      exists <- doesFileExist filePath
      if not exists
        then return $ Left $ "Import file not found: " ++ filePath
        else do
          -- Parse the file
          content <- TIO.readFile filePath
          case parse pProgram filePath content of
            Left err -> return $ Left $ "Failed to parse " ++ filePath ++ ":\n" ++ errorBundlePretty err
            Right (Program defs) -> do
              -- Extract imports and other definitions
              let imports = [imp | DImport imp <- defs]
              let nonImports = [d | d <- defs, not (isImport d)]
              
              -- Recursively load imported files
              let baseDir = takeDirectory filePath
              importedPrograms <- mapM (\imp -> loadImport baseDir imp newVisited) imports
              
              -- Check for errors in imports
              let errors = [err | Left err <- importedPrograms]
              if not (null errors)
                then return $ Left $ unlines errors
                else do
                  -- Merge all programs
                  let importedDefs = concat [defs' | Right (Program defs') <- importedPrograms]
                  return $ Right $ Program (importedDefs ++ nonImports)

-- | Load a single import declaration
loadImport :: FilePath -> ImportDecl -> Set.Set FilePath -> IO (Either String Program)
loadImport baseDir (ImportDecl importPath items) visited = do
  let fullPath = baseDir </> T.unpack importPath
  result <- loadProgramWithImports fullPath visited
  
  case result of
    Left err -> return $ Left err
    Right (Program defs) -> do
      -- Filter definitions based on ImportItems
      let filteredDefs = case items of
            ImportAll -> defs
            ImportSingle name -> filterDefsByName [name] defs
            ImportSelected names -> filterDefsByName names defs
      return $ Right $ Program filteredDefs

-- | Filter definitions to only include specific named procedures
filterDefsByName :: [Text] -> [Definition] -> [Definition]
filterDefsByName names defs = filter matchesName defs
  where
    matchesName (DProc procDef) = procName procDef `elem` names
    matchesName _ = False  -- Don't import statements, only procs

-- | Check if a definition is an import
isImport :: Definition -> Bool
isImport (DImport _) = True
isImport _ = False
