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
        ((modMask, xK_m), launchMemo "~/Obsidian\\ Vault/tweet"),
        ((modMask, xK_s), spawn "maim -s | xclip -selection clipboard -t image/png"),
        ((modMask, xK_p), spawn "maim -s ~/Screenshots/$(date +%s).png")
      ]

removalKeyBindings :: [(ButtonMask, KeySym)]
removalKeyBindings = []

applyCustomKeyBindings :: MyConfig -> XConfig l -> XConfig l
applyCustomKeyBindings myConfig xconfig =
  (xconfig `removeKeys` removalKeyBindings) `additionalKeys` newKeyBindings myConfig
