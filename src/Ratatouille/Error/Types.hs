module Ratatouille.Error.Types
  ( RichError(..)
  , makeRichError
  ) where

import qualified Data.Text as T
import Ratatouille.Error.Context (extractErrorContext, getErrorLine)

-- Rich error that wraps the raw parser output with contextual info
data RichError = RichError
  { reFile :: FilePath
  , reRaw  :: String
  , reLine :: Int
  , reContexts :: [String]
  }

makeRichError :: FilePath -> String -> T.Text -> RichError
makeRichError fp raw content =
  let line = getErrorLine raw
      contexts = extractErrorContext content line
  in RichError { reFile = fp, reRaw = raw, reLine = line, reContexts = contexts }
