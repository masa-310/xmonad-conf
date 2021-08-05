module XMobar (xmobarLogHook, spawnXmobar) where

import Data.List
import XMonad.Config.Prime (WindowSpace, X)
import XMonad.StackSet (Workspace(..))
import XMonad.Hooks.DynamicLog 
import XMonad.Util.WorkspaceCompare (getSortByTag, mkWsSort, WorkspaceCompare, WorkspaceSort)
import XMonad.Util.Run
import GHC.IO.Handle

{-
ppSort_ :: [WindowSpace] -> [WindowSpace]
ppSort_ ws =  sortBy (\w1 -> \w2 -> 
    case (w1, w2) of
      (Workspace i1 _ _, Workspace i2 _ _) -> 
        let ind1 = (read::String->Int) $ [last i1]
            ind2 = (read::String->Int) $ [last i1]
        in if ind1 < ind2 then
              LT
           else if ind1 > ind2 then
              GT
           else
              EQ
  ) ws
-}
ppSort_ :: X WorkspaceSort
ppSort_ = mkWsSort $ return (\i1 -> \i2 ->
        let ind1 = last i1
            ind2 = last i1
        in if ind1 <= ind2 then
              LT
           else 
              GT
        )


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
              , ppSort            =  getSortByTag 
              }

spawnXmobar :: IO Handle
spawnXmobar = spawnPipe xmobarExecName
xmobarLogHook = (\h ->
    dynamicLogWithPP $ wsPP { ppOutput =  hPutStrLn h})
