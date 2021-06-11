module XMobar (xmobarLogHook, spawnXmobar) where

import XMonad.Hooks.DynamicLog 
import XMonad.Util.WorkspaceCompare (getSortByTag)
import XMonad.Util.Run
import GHC.IO.Handle

xmobarExecName = "xmobar"
wsPP = xmobarPP { ppOrder           = \(ws:l:_:_)  -> [ws]
              , ppCurrent         = xmobarColor "red"     "black" . \s -> s
              , ppVisible         = xmobarColor "orange"  "black" . \s -> s
              , ppUrgent          = xmobarColor "gray"    "black" . \s -> s
              , ppHidden          = xmobarColor "gray"    "black" . \s -> s
              , ppHiddenNoWindows = xmobarColor "gray"    "black" . \s -> s
              , ppOutput          = putStrLn
              , ppWsSep           = " "
              , ppSep             = "  "
              , ppSort            = getSortByTag
              }

spawnXmobar :: IO Handle
spawnXmobar = spawnPipe xmobarExecName
xmobarLogHook = (\h ->
    dynamicLogWithPP $ wsPP { ppOutput =  hPutStrLn h})
