module Ratatouille.Error.Format
  ( formatError
  , formatErrorWithContext
  , formatRichError
  , colorize
  ) where

import Data.List (isPrefixOf)
import Ratatouille.Error.Types (RichError(..))

-- Simple ANSI color codes (no emojis per request)
reset, bold, red, green, yellow, blue, magenta, cyan, white, dim :: String
reset = "\ESC[0m"
bold = "\ESC[1m"
red = "\ESC[31m"
green = "\ESC[32m"
yellow = "\ESC[33m"
blue = "\ESC[34m"
magenta = "\ESC[35m"
cyan = "\ESC[36m"
white = "\ESC[97m"
dim = "\ESC[2m"

-- Colorize helper
colorize :: String -> String -> String
colorize color text = color ++ text ++ reset

-- Format error with context lines
formatErrorWithContext :: String -> [String] -> String
formatErrorWithContext errMsg contexts =
  if null contexts then errMsg else unlines (
    [ colorize (bold ++ yellow) "Error Context:" ] ++ map (\c -> colorize (bold ++ cyan) ("  - " ++ c)) contexts ++ ["", errMsg]
  )

-- Format a RichError
formatRichError :: RichError -> String
formatRichError re =
  let header = colorize (bold ++ red) ("Parse error in " ++ reFile re ++ ":" ++ show (reLine re))
      contextBlock = if null (reContexts re) then "" else unlines $ map ("  - " ++) (reContexts re)
  in unlines [header, contextBlock, reRaw re]

-- Basic error formatter: color noticeable parts of the parse error output
formatError :: String -> String
formatError errMsg = unlines $ map colorLine (lines errMsg)
  where
    colorLine line
      | any (== ':') line && any (`elem` "0123456789") (take 10 line) && not ('|' `elem` line) =
          let (file, rest) = break (== ':') line
              (lineNum, rest2) = break (== ':') (drop 1 rest)
          in colorize (bold ++ cyan) " at " ++ colorize (bold ++ blue) file ++ colorize dim ":" ++ colorize (bold ++ yellow) lineNum ++ colorize dim ":" ++ colorize (bold ++ yellow) (drop 1 rest2)
      | all (`elem` " 0123456789:|") line && '|' `elem` line =
          let (lineNum, rest) = break (== '|') line
              code = drop 1 rest
          in colorize (bold ++ magenta) lineNum ++ colorize dim "|" ++ colorize white code
      | "unexpected" `isPrefixOf` line = colorize (bold ++ red) "UNEXPECTED: " ++ colorize red (drop 11 line)
      | "expecting" `isPrefixOf` line = colorize (bold ++ green) "EXPECTED: " ++ colorize yellow (drop 10 line)
      | otherwise = colorize white ("  " ++ line)
