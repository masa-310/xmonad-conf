import Control.Monad
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time (getZonedTime)

import XMonad -- additionalKeysP
import XMonad.Util.EZConfig (additionalKeys)

-- layout
import XMonad.Layout (Full )
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeColMid))
import XMonad.Layout.TwoPane (TwoPane(TwoPane))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.Spacing (spacingRaw, Border(Border))
import XMonad.Layout.Gaps
import XMonad.Layout.WorkspaceDir (workspaceDir, changeDir)
import XMonad.Layout.Drawer (onLeft, simpleDrawer)
import XMonad.Layout.Dishes (Dishes(Dishes))
import XMonad.Layout.DragPane
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.AppLauncher (launchApp)
import XMonad.Prompt.AppendFile
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

-- Action
import XMonad.Actions.Search
import XMonad.Actions.DynamicProjects (Project(..), dynamicProjects, switchProjectPrompt, shiftToProjectPrompt, switchProject)

import XMonad.Hooks.ManageDocks (avoidStruts, docks)

import XMonad.Util.Replace (replace)
import XMonad.Util.WindowProperties (Property(ClassName, Or))


import XMonad.Config.Desktop (desktopConfig)

import Control.Concurrent (threadDelay)

import XMobar (xmobarLogHook, spawnXmobar)

main :: IO()

gap_ = 0
borderWidth_ = 0

terminal_ = "lxterminal"
myfont="xft:DejaVu Sans Mono:alias=true:size=11:hinting=true,xft:Symbola,xft:Noto Color Emoji"

modMask_ = mod4Mask
-- workspaces_ = map show [1..9]
workspaces_ =  []


layoutHook_ =
  workspaceDir "~"
    $ avoidStruts
    $ toggleLayouts (noBorders Full)
    $ spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True
    $ maximizeWithPadding 10 (ResizableTall 1 (3/100) (3/5) []
-- can use something useful?
--  $ drawer `onLeft` (ResizableTall 1 (3/100) (3/5) [])
--  ||| Dishes 2 (1/6)
--  ||| dragPane Horizontal 0.1 0.5
      ||| (ThreeColMid 1 (3/100) (1/2))
      ||| (TwoPane (3/100) (3/5))
      ||| Full)
  where drawer = simpleDrawer 0.01 0.3 (ClassName "Rhythmbox" `Or` ClassName "Xchat")

startupHook_ = do
  spawn "autorandr -l .autorandr"
  spawn "feh --bg-center .wallpaper/wallpaper.png"
  spawn "wmname LG3D"
  switchProject $ head projects

prompt_conf = def
    { promptBorderWidth = 1
    , alwaysHighlight = True
    , height = 22
    , historySize = 256
    , font = myfont
    -- , bgColor = myBackgroundColor
    -- , fgColor = myContentColor
    -- , bgHLight = myBackgroundColor
    -- , fgHLight = myContentColor
    -- , borderColor = myBackgroundColor
    , position = Bottom
    , autoComplete = Just 100
    , showCompletionOnTab = False
    , searchPredicate = fuzzyMatch
    , defaultPrompter = id
    , sorter = const id
    , maxComplRows = Just 7
    , promptKeymap = defaultXPKeymap
    , completionKey = (0, xK_Tab)
    , changeModeKey = xK_grave
    , historyFilter = id
    , defaultText = []
    }

googleChrome = "/usr/bin/google-chrome-stable"

projects :: [Project]
projects = [
  Project {
    projectName = "1_home"
    , projectDirectory = "~"
    , projectStartHook = Nothing
    }
  , Project {
    projectName = "2_chrome"
    , projectDirectory = "~/Downloads"
    , projectStartHook = Just $ do
        spawn "google-chrome-stable"
    }
  , Project {
    projectName = "3_app"
    , projectDirectory = "~/project/douzo-with-app"
    , projectStartHook = Just $ do
        spawn "lxterminal --command tmux"
    }
  , Project {
    projectName = "4_backend"
    , projectDirectory = "~/project/douzo-with-backend"
    , projectStartHook = Just $ do
        spawn "lxterminal --command tmux"
    }
  , Project {
    projectName = "5_cms"
    , projectDirectory = "~/project/douzo-cms"
    , projectStartHook = Just $ do
        spawn "lxterminal --command tmux"
    }
  , Project {
    projectName = "8_notion"
    , projectDirectory = "~"
    , projectStartHook = Just $ do
        spawn "notion-app"
    }
  , Project {
    projectName = "9_slack"
    , projectDirectory = "~"
    , projectStartHook = Just $ do
        spawn "slack"
    }
  ]

main = do
  replace
  xmobarProc <- spawnXmobar
  xmonad 
     $ (docks $ dynamicProjects projects $ def {
      terminal        = terminal_
      , modMask       = modMask_
      , layoutHook    = layoutHook_
      , startupHook   = startupHook_
      , logHook = xmobarLogHook xmobarProc
      , workspaces = workspaces_
    } `additionalKeys` [
    ((modMask_, xK_c), changeDir prompt_conf )
    , ((modMask_, xK_f), withFocused (sendMessage . maximizeRestore))
    -- , ((modMask_, xK_h), launchApp def "feh" )
    -- , ((modMask_, xK_e), launchApp prompt_conf "evince" )
    , ((modMask_, xK_n), do 
      date <- io $ liftM (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
      appendFilePrompt' prompt_conf (date ++) $ "~/NOTES"
    )
    , ((modMask_, xK_g), promptSearchBrowser (greenXPConfig { font = myfont }) googleChrome google)
    , ((modMask_, xK_d), spawn "dmenu_run")
    , ((modMask_, xK_space), switchProjectPrompt prompt_conf)
    , ((modMask_, xK_slash), shiftToProjectPrompt prompt_conf)

    ])
