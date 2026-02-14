module Keymap (applyCustomKeyBindings) where

import Config (MyConfig, getFont, getModMask, getPromptConf)
import Data.Map as Map
import Memo (launchMemo)
import XMonad
import XMonad.Actions.DynamicProjects (shiftToProjectPrompt, switchProjectPrompt)
import XMonad.Actions.Search (google, promptSearchBrowser)
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Layout.WorkspaceDir (changeDir)
import XMonad.Prompt (XPConfig (font), greenXPConfig)
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings, removeKeys)

googleChrome = "/usr/bin/google-chrome-stable"

newKeyBindings :: MyConfig -> [((ButtonMask, KeySym), X ())]
newKeyBindings myConf =
  let promptConf = getPromptConf myConf
      font = getFont myConf
      modMask = getModMask myConf
   in [ ((modMask, xK_c), changeDir promptConf),
        ((modMask, xK_f), withFocused (sendMessage . maximizeRestore)),
        ((modMask, xK_g), promptSearchBrowser (greenXPConfig {font = font}) googleChrome google),
        ((modMask, xK_d), spawn "rofi -show run"),
        ((modMask, xK_z), spawn "zeal"),
        ((modMask, xK_space), switchProjectPrompt promptConf),
        ((modMask, xK_slash), shiftToProjectPrompt promptConf),
        ((modMask, xK_z), spawn "wezterm-gui start --class __temp-term-top"),
        ((modMask .|. shiftMask, xK_z), spawn "wezterm-gui start --class __temp-term-bottom"),
        ((modMask, xK_3), spawn "maim -s ~/Screenshots/$(date +%Y-%m-%dT%T).png"),
        ((modMask .|. shiftMask, xK_3), spawn "file=${HOME}/Screenshots/$(date +%Y-%m-%dT%T).png && maim -s $file && drawing $file"),
        ((modMask , xK_4), spawn "maim -s | xclip -selection clipboard -t image/png"),
        ((modMask .|. shiftMask, xK_4), spawn "maim -s | xclip -selection clipboard -t image/png && drawing -c"),
        ((modMask, xK_5), spawn "peek"),
        ((modMask, xK_F5), spawn "brightnessctl set 10%-"),
        ((modMask, xK_F6), spawn "brightnessctl set 10%+"),
        ((modMask, xK_7), spawn "pamixer -d 5"),
        ((modMask, xK_8), spawn "pamixer -i 5"),
        ((modMask, xK_9), spawn "pamixer -t")
      ]

removalKeyBindings :: MyConfig -> [(ButtonMask, KeySym)]
removalKeyBindings myConf =
  let modMask = getModMask myConf
  in [ ]

applyCustomKeyBindings :: MyConfig -> XConfig l -> XConfig l
applyCustomKeyBindings myConfig xconfig =
  (xconfig `removeKeys` removalKeyBindings myConfig) `additionalKeys` newKeyBindings myConfig
