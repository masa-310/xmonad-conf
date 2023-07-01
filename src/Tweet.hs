module Tweet (mkTweetPrompt) where

import XMonad (X)
import XMonad.Prompt as Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

mkTweetPrompt :: X ()
mkTweetPrompt =
  mkXPrompt Tweet promptConfig complFun $ \_ -> return ()

promptConfig :: XPConfig
promptConfig =
  def
    { font = "Inconsolata Nerd Font Mono",
      autoComplete = Just 100,
      searchPredicate = fuzzyMatch,
      bgColor = "#555",
      fgColor = "#fff",
      borderColor = "#fff",
      promptBorderWidth = 2,
      position = CenteredAt 0.5 0.5,
      height = 360,
      showCompletionOnTab = True
    }

data Tweet = Tweet

instance XPrompt Tweet where
  showXPrompt Tweet = "Tweet: "

complFun :: ComplFunction
complFun = mkComplFunFromList promptConfig []
