import Control.Monad
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time (getZonedTime)

import XMonad
-- additionalKeysP
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
modMask_ = mod4Mask
workspaces_ = map show [1..9]

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
  spawn "autorandr -l .autorandr && feh --bg-fill .wallpaper/wallpaper.png"
  spawn "wmname LG3D"

main = do
  replace
  xmobarProc <- spawnXmobar
  xmonad $ (docks def {
      terminal        = terminal_
      , modMask       = modMask_
      , layoutHook    = layoutHook_
      , startupHook   = startupHook_
      , logHook = xmobarLogHook xmobarProc
    } `additionalKeys` [
    ((modMask_, xK_c), changeDir def)
    , ((modMask_, xK_f), withFocused (sendMessage . maximizeRestore))
    -- , ((modMask_, xK_h), launchApp def "feh" )
    , ((modMask_, xK_e), launchApp def "evince" )
    , ((modMask_, xK_n), do 
      date <- io $ liftM (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
      appendFilePrompt' def (date ++) $ "/home/masashi/NOTES"
    )
    , ((modMask_, xK_d), spawn "dmenu_run")

    ])
