{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- VM entry point - Execute bytecode files
-}

module Main (main) where

import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar
  , newTQueue
  , newTVarIO
  )
import Data.List (isInfixOf)
import qualified Data.Map as Map
import qualified Data.Text as T
import Ratatouille.Bytecode.Decoder (readBinaryFile)
import Ratatouille.Bytecode.Types (Bytecode, Value (..))
import Ratatouille.VM.Interpreter (executeBytecode)
import Ratatouille.VM.VM
  ( Pid (..)
  , Process (..)
  , VMError (..)
  , VMState (..)
  , executeVM
  )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)


-- | Main entry point for the VM
main :: IO ()
main = getArgs >>= handleArgs

-- | Handle command-line arguments
handleArgs :: [String] -> IO ()
handleArgs [] = printUsage >> exitSuccess
handleArgs ["--help"] = printUsage >> exitSuccess
handleArgs ["--repl"] = printReplError >> exitFailure
handleArgs ["--debug"] = printDebugError >> exitFailure
handleArgs ["--trace"] = printTraceError >> exitFailure
handleArgs ["--debug", file] =
  runFileWithOptions file True False >> exitSuccess
handleArgs ["--trace", file] =
  runFileWithOptions file False True >> exitSuccess
handleArgs [file] = runFile file >> exitSuccess
handleArgs _ = printInvalidArgs >> exitFailure

-- | Print REPL not available error
printReplError :: IO ()
printReplError = putStrLn "REPL mode not available in this version"

-- | Print debug option error
printDebugError :: IO ()
printDebugError = putStrLn "Error: --debug requires a bytecode file"

-- | Print trace option error
printTraceError :: IO ()
printTraceError = putStrLn "Error: --trace requires a bytecode file"

-- | Print invalid arguments error
printInvalidArgs :: IO ()
printInvalidArgs = do
  putStrLn "Error: Invalid arguments"
  printUsage

-- | Create initial VM state with main process
createInitialVMState :: Bytecode -> IO VMState
createInitialVMState bytecode = do
  processesVar <- newTVarIO Map.empty
  nextPidVar <- newTVarIO 1
  mainProcess <- createMainProcess bytecode
  atomically $ modifyTVar processesVar
    (Map.insert (Pid 0) mainProcess)
  return $ buildVMState bytecode processesVar nextPidVar

-- | Create the main process (Pid 0) with mailbox
createMainProcess :: Bytecode -> IO Process
createMainProcess bytecode = do
  mainMailbox <- atomically newTQueue
  return Process
    { processId = Pid 0
    , processStack = []
    , processLocals = Map.empty
    , processState = VUnit
    , processMailbox = mainMailbox
    , processPc = 0
    , processBytecode = bytecode
    , processThreadId = Nothing
    }

-- | Build VM state with initialized components
buildVMState :: Bytecode -> TVar (Map.Map Pid Process)
             -> TVar Pid -> VMState
buildVMState bytecode processesVar nextPidVar =
  VMState
    { vmStack = []
    , vmGlobals = Map.empty
    , vmLocals = Map.empty
    , vmPc = 0
    , vmBytecode = bytecode
    , vmLabels = Map.empty
    , vmProcessDefs = Map.empty
    , vmFunctionDefs = Map.empty
    , vmProcesses = processesVar
    , vmNextPid = nextPidVar
    , vmCurrentPid = Just (Pid 0)
    , vmDebugMode = False
    , vmBreakpoints = []
    , vmTraceEnabled = False
    }


-- | Print usage information
printUsage :: IO ()
printUsage = putStrLn $ unlines
  [ "GLaDOS Virtual Machine"
  , ""
  , "Usage: glados-vm [OPTIONS] [FILE]"
  , ""
  , "Options:"
  , "  --repl           Start interactive REPL mode"
  , "  --debug FILE     Run FILE with debug mode enabled"
  , "  --trace FILE     Run FILE with instruction tracing"
  , "  --help           Show this help message"
  , ""
  , "Examples:"
  , "  glados-vm program.bc          # Run bytecode from file"
  , "  glados-vm --repl              # Start REPL"
  , "  glados-vm --debug program.bc  # Run with debugging"
  , "  glados-vm --trace program.bc  # Run with tracing"
  ]

-- | Run bytecode from a file with default options
runFile :: FilePath -> IO ()
runFile file = runFileWithOptions file False False

-- | Run bytecode from a file with debug and trace options
runFileWithOptions :: FilePath -> Bool -> Bool -> IO ()
runFileWithOptions file debugMode traceMode = do
  bytecodeResult <- readBinaryFile file
  case bytecodeResult of
    Left err -> handleBytecodeError err
    Right bytecode -> executeBytecodeFile bytecode debugMode traceMode

-- | Handle bytecode reading error
handleBytecodeError :: String -> IO ()
handleBytecodeError err = do
  putStrLn $ "Error reading bytecode: " ++ err
  exitFailure

-- | Execute bytecode file with given options
executeBytecodeFile :: Bytecode -> Bool -> Bool -> IO ()
executeBytecodeFile bytecode debugMode traceMode = do
  vmState <- createInitialVMState bytecode
  let vmState' = vmState { vmDebugMode = debugMode
                         , vmTraceEnabled = traceMode }
  (result, _finalState) <- executeVM vmState' (executeBytecode bytecode)
  handleExecutionResult result

-- | Handle VM execution result
handleExecutionResult :: Either VMError Value -> IO ()
handleExecutionResult (Left (ProcessError msg))
  | "No message in mailbox" `isInfixOf` T.unpack msg = exitSuccess
handleExecutionResult (Left err) = do
  putStrLn $ "Error: " ++ show err
  exitFailure
handleExecutionResult (Right _value) = exitSuccess
