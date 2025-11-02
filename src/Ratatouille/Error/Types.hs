{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Error types with rich context information
-}

module Ratatouille.Error.Types
  ( RichError (..)
  , makeRichError
  )
where

import qualified Data.Text as T
import Ratatouille.Error.Context (extractErrorContext, getErrorLine)

-- | Rich error that wraps raw parser output with contextual information
-- Provides file location, line number, and code context for better errors
data RichError = RichError
  { reFile :: FilePath      -- | Source file path
  , reRaw :: String          -- | Raw error message from parser
  , reLine :: Int            -- | Line number where error occurred
  , reContexts :: [String]   -- | Contextual information (proc, block, etc.)
  }

-- | Create a rich error from parse error components
-- Extracts line number and context from the source code
makeRichError :: FilePath -> String -> T.Text -> RichError
makeRichError fp raw content =
  let line = getErrorLine raw
      contexts = extractErrorContext content line
  in RichError
       { reFile = fp
       , reRaw = raw
       , reLine = line
       , reContexts = contexts
       }
