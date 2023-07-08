module Memo (launchMemo) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, iso8601DateFormat, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import XMonad (MonadIO (liftIO), X, spawn)
import XMonad.Hooks.Place (Placement, placeFocused, smart, withGaps)
import XMonad.Prelude (mkAbsolutePath)
import XMonad.Prompt as Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Util.Run (execute, inTerm, proc, setXClass, (>->))

placement :: Placement
placement = withGaps (16, 16, 16, 16) (smart (0.5, 0.5))

launchMemo :: String -> X ()
launchMemo memoPath = do
  utc <- liftIO getCurrentTime
  let unix = (floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds) utc
      path = memoPath ++ "/" ++ show unix ++ ".md"
  -- open vi to have an immediate memo as md format
  proc $ inTerm >-> setXClass "tweet" >-> execute ("vi " ++ path)
  -- write a frontmatter
  liftIO $
    mkAbsolutePath path >>= \path -> do
      readFile path >>= \content -> do
        let created_at = iso8601Show utc
        writeFile path "---\n"
        appendFile path $ "created_at: " ++ created_at ++ "\n"
        appendFile path $ "updated_at: " ++ created_at ++ "\n"
        appendFile path "---\n"
        appendFile path content
