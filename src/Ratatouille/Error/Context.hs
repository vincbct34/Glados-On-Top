{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Error context extraction from source code
-}

module Ratatouille.Error.Context
  ( extractErrorContext,
    getErrorLine,
  )
where

import Data.Char (isDigit)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.Maybe (catMaybes)
import qualified Data.Text as T

-- | Extract contextual information about where the error occurred
-- Returns a hierarchy of contexts (procedure, block, statement)
extractErrorContext :: T.Text -> Int -> [String]
extractErrorContext content errorLine =
  let sourceLines = T.lines content
      currentLine = getCurrentLine sourceLines errorLine
      previousLines = take errorLine (map T.unpack sourceLines)
      contexts = buildContextHierarchy previousLines currentLine
   in if null contexts then ["in top-level code"] else contexts

-- | Get the current line from source
getCurrentLine :: [T.Text] -> Int -> String
getCurrentLine sourceLines errorLine
  | errorLine > 0 && errorLine <= length sourceLines =
      T.unpack (sourceLines !! (errorLine - 1))
  | otherwise = ""

-- | Build a hierarchy of contexts (outermost to innermost)
buildContextHierarchy :: [String] -> String -> [String]
buildContextHierarchy previousLines currentLine =
  let procCtx = findMostRecentProc previousLines
      blockCtxs = findBlockContexts previousLines
      stmtCtx = findStatementContext previousLines currentLine
      allContexts =
        catMaybes [procCtx]
          ++ blockCtxs
          ++ catMaybes [stmtCtx]
   in if null allContexts then [] else allContexts

-- | Find the most recent procedure definition
findMostRecentProc :: [String] -> Maybe String
findMostRecentProc sourceLines =
  let reversedLines = reverse sourceLines
      procLine = find (\l -> "proc " `isPrefixOf` dropWhile (== ' ') l) reversedLines
   in extractProcName <$> procLine

-- | Extract procedure name from proc definition line
extractProcName :: String -> String
extractProcName line =
  let trimmed = dropWhile (== ' ') line
      afterProc = drop 5 trimmed
      procName = takeWhile (\c -> c /= '(' && c /= ' ' && c /= ':') afterProc
   in if null procName
        then "in a procedure definition"
        else "in procedure '" ++ procName ++ "'"

-- | Find all block contexts we're nested in
findBlockContexts :: [String] -> [String]
findBlockContexts sourceLines =
  let reversed = reverse sourceLines
      receiveCtx =
        checkBlockContext
          reversed
          "receive"
          "inside a receive block"
      matchCtx =
        checkBlockContext
          reversed
          "match"
          "inside a match expression"
      loopCtx = checkBlockContext reversed "loop" "inside a loop"
      ifCtx = checkBlockContext reversed "if" "inside an if expression"
   in catMaybes [receiveCtx, matchCtx, loopCtx, ifCtx]

-- | Check if a block context exists
checkBlockContext :: [String] -> String -> String -> Maybe String
checkBlockContext reversed keyword contextMsg =
  if hasUnclosedBlock reversed keyword
    then Just contextMsg
    else Nothing

-- | Check if we have an unclosed block for a keyword
hasUnclosedBlock :: [String] -> String -> Bool
hasUnclosedBlock reversed keyword
  | not (any (isInfixOf keyword) reversed) = False
  | otherwise = braceCountExceeds reversed keyword

-- | Check if open braces exceed close braces from keyword
braceCountExceeds :: [String] -> String -> Bool
braceCountExceeds reversed keyword =
  let idx = findIndexInList (isInfixOf keyword) reversed 0
      linesFromKeyword = take (idx + 1) reversed
      openBraces = sum $ map (length . filter (== '{')) linesFromKeyword
      closeBraces = sum $ map (length . filter (== '}')) linesFromKeyword
   in openBraces > closeBraces

-- | Find index of first occurrence in list
findIndexInList :: (a -> Bool) -> [a] -> Int -> Int
findIndexInList _ [] i = i - 1
findIndexInList p (x : xs) i
  | p x = i
  | otherwise = findIndexInList p xs (i + 1)

-- | Find statement-level context
findStatementContext :: [String] -> String -> Maybe String
findStatementContext previousLines currentLine =
  let reversed = reverse previousLines
      stateCtx = checkLineContext reversed "state:" "in state initialization"
      letCtx = checkLineContext reversed "let " "in let binding"
      sendCtx = checkLineContext reversed "send " "in send statement"
      spawnCtx = checkLineContext reversed "spawn " "in spawn expression"
      currentTrimmed = dropWhile (== ' ') currentLine
      currentCtx = analyzeCurrentLine currentTrimmed
   in currentCtx
        `orElse` stateCtx
        `orElse` letCtx
        `orElse` sendCtx
        `orElse` spawnCtx
  where
    orElse Nothing y = y
    orElse x _ = x

-- | Check if any line matches a context pattern
checkLineContext :: [String] -> String -> String -> Maybe String
checkLineContext reversed prefix contextMsg =
  if any (\l -> prefix `isPrefixOf` dropWhile (== ' ') l) reversed
    then Just contextMsg
    else Nothing

-- | Analyze current line for context
analyzeCurrentLine :: String -> Maybe String
analyzeCurrentLine trimmed
  | "case " `isPrefixOf` trimmed = Just "in case pattern"
  | "=>" `isInfixOf` trimmed = Just "in case body"
  | "->" `isInfixOf` trimmed = Just "in function type signature"
  | ':' `elem` trimmed && not ("//" `isInfixOf` trimmed) =
      Just "in type annotation"
  | otherwise = Nothing

-- | Get error line number from parse error message
getErrorLine :: String -> Int
getErrorLine errMsg =
  let errorLines = lines errMsg
      locationLine = find (\l -> ':' `elem` l && any isDigit l) errorLines
   in maybe 1 parseLineNumber locationLine

-- | Parse line number from error location string
parseLineNumber :: String -> Int
parseLineNumber line =
  let parts = words line
      numPart = find (all (\c -> isDigit c || c == ':')) parts
   in maybe 1 extractLineNumber numPart

-- | Extract line number from numeric string
extractLineNumber :: String -> Int
extractLineNumber nums =
  let lineNum = takeWhile isDigit $ dropWhile (not . isDigit) nums
   in if null lineNum then 1 else read lineNum
