module ManageHook (customManageHook) where

import Data.Monoid (Endo)
import XMonad.Core (Query, WindowSet)
import XMonad.Hooks.ManageHelpers (doRectFloat, doCenterFloat, doSideFloat, Side(NC, SC))
import XMonad.ManageHook (className, composeAll, doFloat, (-->), (=?))
import XMonad.StackSet (RationalRect (RationalRect))

customManageHook :: Query (Endo WindowSet)
customManageHook =
  composeAll
    [ className =? "Peek" --> doCenterFloat
    , className =? "Thunar" --> doSideFloat NC
    , className =? "com.github.maoschanz.drawing" --> doSideFloat NC
    , className =? "pavucontrol" --> doSideFloat NC
    , className =? "__temp-term-bottom" --> doSideFloat SC
    , className =? "__temp-term-top" --> doSideFloat NC
    ]
