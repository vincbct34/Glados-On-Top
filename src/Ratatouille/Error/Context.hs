module Ratatouille.Error.Context
  ( extractErrorContext
  , getErrorLine
  ) where

import qualified Data.Text as T
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Char (isDigit)
import Data.List (isPrefixOf, isInfixOf)

-- Extract contextual information about where the error occurred
extractErrorContext :: T.Text -> Int -> [String]
extractErrorContext content errorLine =
  let sourceLines = T.lines content
      currentLine = if errorLine > 0 && errorLine <= length sourceLines
                    then T.unpack (sourceLines !! (errorLine - 1))
                    else ""
      previousLines = take errorLine (map T.unpack sourceLines)
      contexts = buildContextHierarchy previousLines currentLine
  in if null contexts then ["in top-level code"] else contexts

-- Build a hierarchy of contexts (outermost to innermost)
buildContextHierarchy :: [String] -> String -> [String]
buildContextHierarchy previousLines currentLine =
  let procCtx = findMostRecentProc previousLines
      blockCtxs = findBlockContexts previousLines
      stmtCtx = findStatementContext previousLines currentLine
      allContexts = mapMaybe id [procCtx] ++ blockCtxs ++ mapMaybe id [stmtCtx]
  in if null allContexts then [] else allContexts

-- Find the most recent procedure definition with its full signature
findMostRecentProc :: [String] -> Maybe String
findMostRecentProc sourceLines =
  let reversedLines = reverse sourceLines
      procLine = listToMaybe $ filter (\l -> "proc " `isPrefixOf` dropWhile (== ' ') l) reversedLines
  in case procLine of
       Just line ->
         let trimmed = dropWhile (== ' ') line
             afterProc = drop 5 trimmed
             procName = takeWhile (\c -> c /= '(' && c /= ' ' && c /= ':') afterProc
         in if null procName then Just "in a procedure definition"
            else Just $ "in procedure '" ++ procName ++ "'"
       Nothing -> Nothing

-- Find all block contexts we're nested in
findBlockContexts :: [String] -> [String]
findBlockContexts sourceLines =
  let reversed = reverse sourceLines
      receiveCtx = if hasUnclosedBlock reversed "receive" then Just "inside a receive block" else Nothing
      matchCtx = if hasUnclosedBlock reversed "match" then Just "inside a match expression" else Nothing
      loopCtx = if hasUnclosedBlock reversed "loop" then Just "inside a loop" else Nothing
      ifCtx = if hasUnclosedBlock reversed "if" then Just "inside an if expression" else Nothing
      allCtxs = mapMaybe id [receiveCtx, matchCtx, loopCtx, ifCtx]
  in allCtxs

-- Check if we have an unclosed block for a keyword
hasUnclosedBlock :: [String] -> String -> Bool
hasUnclosedBlock reversed keyword =
  if not (any (isInfixOf keyword) reversed)
  then False
  else
    let -- find index of first occurrence of keyword in reversed list
        idx = findIndexInList (isInfixOf keyword) reversed 0
        linesFromKeyword = take (idx + 1) reversed
        openBraces = sum $ map (length . filter (== '{')) linesFromKeyword
        closeBraces = sum $ map (length . filter (== '}')) linesFromKeyword
    in openBraces > closeBraces
  where
    findIndexInList _ [] i = i - 1
    findIndexInList p (x:xs) i = if p x then i else findIndexInList p xs (i + 1)

-- Find statement-level context (what kind of statement/expression we're in)
findStatementContext :: [String] -> String -> Maybe String
findStatementContext previousLines currentLine =
  let reversed = reverse previousLines
      stateCtx = if any (\l -> "state:" `isPrefixOf` dropWhile (== ' ') l) reversed then Just "in state initialization" else Nothing
      letCtx = if any (\l -> "let " `isPrefixOf` dropWhile (== ' ') l) reversed then Just "in let binding" else Nothing
      sendCtx = if any (\l -> "send " `isPrefixOf` dropWhile (== ' ') l) reversed then Just "in send statement" else Nothing
      spawnCtx = if any (\l -> "spawn " `isPrefixOf` dropWhile (== ' ') l) reversed then Just "in spawn expression" else Nothing
      currentTrimmed = dropWhile (== ' ') currentLine
      currentCtx
        | "case " `isPrefixOf` currentTrimmed = Just "in case pattern"
        | "=>" `isInfixOf` currentTrimmed = Just "in case body"
        | "->" `isInfixOf` currentTrimmed = Just "in function type signature"
        | ':' `elem` currentTrimmed && not ("//" `isInfixOf` currentTrimmed) = Just "in type annotation"
        | otherwise = Nothing
  in currentCtx `orElse` stateCtx `orElse` letCtx `orElse` sendCtx `orElse` spawnCtx
  where
    orElse Nothing y = y
    orElse x _ = x

-- Get error line number from parse error message
getErrorLine :: String -> Int
getErrorLine errMsg =
  let errorLines = lines errMsg
      locationLine = listToMaybe $ filter (\l -> ':' `elem` l && any isDigit l) errorLines
  in case locationLine of
       Just line -> parseLineNumber line
       Nothing -> 1
  where
    parseLineNumber line =
      let parts = words line
          numPart = listToMaybe $ filter (all (\c -> isDigit c || c == ':')) parts
      in case numPart of
           Just nums -> let lineNum = takeWhile isDigit $ dropWhile (not . isDigit) nums
                        in if null lineNum then 1 else read lineNum
           Nothing -> 1
