{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Compiler entry point - Parse and compile .rat files with imports
-}

module Main (main) where

import Data.Either (lefts)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Ratatouille.AST
  ( Definition (..),
    ImportDecl (..),
    ImportItems (..),
    ProcDefinition (..),
    Program (..),
  )
import Ratatouille.Bytecode.Compiler (compileProgram)
import Ratatouille.Bytecode.Encoder (writeBinaryFile)
import Ratatouille.Parser.Proc (pProgram)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (replaceExtension, takeDirectory, (</>))
import Text.Megaparsec (errorBundlePretty, parse)

-- | Main entry point for the compiler with import resolution
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printUsageAndExit
    _ -> compileWithArgs args

-- | Print usage and exit with failure
printUsageAndExit :: IO ()
printUsageAndExit =
  putStrLn "Usage: Glados-On-Top-exe <file.rat> [-o output.rtbc]"
    >> putStrLn
      "       Parse and compile a Ratatouille source file \
      \to binary bytecode"
    >> putStrLn ""
    >> putStrLn "Options:"
    >> putStrLn
      "  -o <file>    Specify output bytecode file \
      \(default: <input>.rtbc)"
    >> exitFailure

-- | Compile files with parsed arguments
compileWithArgs :: [String] -> IO ()
compileWithArgs args =
  let (inputFile, outputFile) = parseArgs args
   in processFile inputFile outputFile

-- | Parse command-line arguments into input/output file paths
parseArgs :: [String] -> (FilePath, FilePath)
parseArgs [input] = (input, replaceExtension input ".rtbc")
parseArgs [input, "-o", output] = (input, output)
parseArgs (input : "-o" : output : _) = (input, output)
parseArgs (input : _) = (input, replaceExtension input ".rtbc")
parseArgs _ = error "Invalid arguments"

-- | Process file by loading imports, compiling, and writing output
processFile :: FilePath -> FilePath -> IO ()
processFile inputPath outputPath = do
  result <- loadProgramWithImports inputPath Set.empty
  case result of
    Left err -> handleCompilationError err
    Right mergedProgram -> compileAndWrite inputPath outputPath mergedProgram

-- | Handle compilation error
handleCompilationError :: String -> IO ()
handleCompilationError err =
  putStrLn ("Error: " ++ err) >> exitFailure

-- | Compile merged program and write to output file
compileAndWrite :: FilePath -> FilePath -> Program -> IO ()
compileAndWrite inputPath outputPath mergedProgram =
  let bytecode = compileProgram mergedProgram
   in writeBinaryFile outputPath bytecode
        >> putStrLn
          ( "Compiled successfully: "
              ++ inputPath
              ++ " -> "
              ++ outputPath
          )
        >> exitSuccess

-- | Load a program and recursively resolve all imports
-- Returns a merged Program with all imported definitions
loadProgramWithImports ::
  FilePath ->
  Set.Set FilePath ->
  IO (Either String Program)
loadProgramWithImports filePath visited
  | filePath `Set.member` visited =
      return $ Left $ "Circular import detected: " ++ filePath
  | otherwise = loadProgramFile filePath visited

-- | Load program file and resolve its imports
loadProgramFile ::
  FilePath ->
  Set.Set FilePath ->
  IO (Either String Program)
loadProgramFile filePath visited = do
  let newVisited = Set.insert filePath visited
  exists <- doesFileExist filePath
  if not exists
    then return $ Left $ "Import file not found: " ++ filePath
    else parseAndLoadImports filePath newVisited

-- | Parse file and load all its imports
parseAndLoadImports ::
  FilePath ->
  Set.Set FilePath ->
  IO (Either String Program)
parseAndLoadImports filePath visited = do
  content <- TIO.readFile filePath
  case parse pProgram filePath content of
    Left err ->
      return $
        Left $
          "Failed to parse "
            ++ filePath
            ++ ":\n"
            ++ errorBundlePretty err
    Right (Program defs) ->
      loadImportsAndMerge filePath visited defs

-- | Load all imports and merge with current definitions
loadImportsAndMerge ::
  FilePath ->
  Set.Set FilePath ->
  [Definition] ->
  IO (Either String Program)
loadImportsAndMerge filePath visited defs = do
  let imports = [imp | DImport imp <- defs]
      nonImports = [d | d <- defs, not (isImport d)]
      baseDir = takeDirectory filePath
  importedPrograms <- mapM (\imp -> loadImport baseDir imp visited) imports
  mergeImportedPrograms importedPrograms nonImports

-- | Merge imported programs with local definitions
mergeImportedPrograms ::
  [Either String Program] ->
  [Definition] ->
  IO (Either String Program)
mergeImportedPrograms importedPrograms nonImports =
  let errors = lefts importedPrograms
   in if not (null errors)
        then return $ Left $ unlines errors
        else
          let importedDefs =
                concat
                  [defs' | Right (Program defs') <- importedPrograms]
           in return $ Right $ Program (importedDefs ++ nonImports)

-- | Load a single import declaration
loadImport ::
  FilePath ->
  ImportDecl ->
  Set.Set FilePath ->
  IO (Either String Program)
loadImport baseDir (ImportDecl impPath items) visited = do
  let fullPath = baseDir </> T.unpack impPath
  result <- loadProgramWithImports fullPath visited
  case result of
    Left err -> return $ Left err
    Right (Program defs) -> return $ Right $ Program (filterImports items defs)

-- | Filter imported definitions based on import items
filterImports :: ImportItems -> [Definition] -> [Definition]
filterImports ImportAll defs = defs
filterImports (ImportSingle name) defs = filterDefsByName [name] defs
filterImports (ImportSelected names) defs = filterDefsByName names defs

-- | Filter definitions to only include specific named procedures
filterDefsByName :: [Text] -> [Definition] -> [Definition]
filterDefsByName names = filter matchesName
  where
    matchesName (DProc procDef) = procName procDef `elem` names
    matchesName _ = False

-- | Check if a definition is an import
isImport :: Definition -> Bool
isImport (DImport _) = True
isImport _ = False
