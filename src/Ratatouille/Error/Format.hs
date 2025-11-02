{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Error formatting with ANSI colors
-}

module Ratatouille.Error.Format
  ( formatError,
    formatErrorWithContext,
    formatRichError,
    colorize,
  )
where

import Data.List (isPrefixOf)
import Ratatouille.Error.Types (RichError (..))

-- | ANSI color code constants
reset :: String
reset = "\ESC[0m"

bold :: String
bold = "\ESC[1m"

red :: String
red = "\ESC[31m"

green :: String
green = "\ESC[32m"

yellow :: String
yellow = "\ESC[33m"

blue :: String
blue = "\ESC[34m"

magenta :: String
magenta = "\ESC[35m"

cyan :: String
cyan = "\ESC[36m"

white :: String
white = "\ESC[97m"

dim :: String
dim = "\ESC[2m"

-- | Apply color codes to text
colorize :: String -> String -> String
colorize color text = color ++ text ++ reset

-- | Format error message with contextual information
formatErrorWithContext :: String -> [String] -> String
formatErrorWithContext errMsg contexts
  | null contexts = errMsg
  | otherwise =
      unlines $
        [colorize (bold ++ yellow) "Error Context:"]
          ++ map formatContext contexts
          ++ ["", errMsg]
  where
    formatContext c = colorize (bold ++ cyan) ("  - " ++ c)

-- | Format a RichError with all components
formatRichError :: RichError -> String
formatRichError re = unlines [header, contextBlock, reRaw re]
  where
    header =
      colorize
        (bold ++ red)
        ("Parse error in " ++ reFile re ++ ":" ++ show (reLine re))
    contextBlock
      | null (reContexts re) = ""
      | otherwise = unlines $ map ("  - " ++) (reContexts re)

-- | Format parse error with syntax highlighting
formatError :: String -> String
formatError errMsg = unlines $ map colorLine (lines errMsg)
  where
    colorLine line
      | isLocationLine line = formatLocationLine line
      | isCodeLine line = formatCodeLine line
      | "unexpected" `isPrefixOf` line = formatUnexpected line
      | "expecting" `isPrefixOf` line = formatExpecting line
      | otherwise = colorize white ("  " ++ line)

    isLocationLine line =
      elem ':' line
        && any (`elem` "0123456789") (take 10 line)
        && notElem '|' line

    isCodeLine line =
      all (`elem` " 0123456789:|") line && '|' `elem` line

    formatLocationLine line =
      let (file, rest) = break (== ':') line
          (lineNum, rest2) = break (== ':') (drop 1 rest)
       in colorize (bold ++ cyan) " at "
            ++ colorize (bold ++ blue) file
            ++ colorize dim ":"
            ++ colorize (bold ++ yellow) lineNum
            ++ colorize dim ":"
            ++ colorize (bold ++ yellow) (drop 1 rest2)

    formatCodeLine line =
      let (lineNum, rest) = break (== '|') line
          code = drop 1 rest
       in colorize (bold ++ magenta) lineNum
            ++ colorize dim "|"
            ++ colorize white code

    formatUnexpected line =
      colorize (bold ++ red) "UNEXPECTED: "
        ++ colorize red (drop 11 line)

    formatExpecting line =
      colorize (bold ++ green) "EXPECTED: "
        ++ colorize yellow (drop 10 line)
