module ManageHook (customManageHook) where

import Data.Monoid (Endo)
import XMonad.Core (Query, WindowSet)
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.ManageHook (className, composeAll, doFloat, (-->), (=?))
import XMonad.StackSet (RationalRect (RationalRect))

customManageHook :: Query (Endo WindowSet)
customManageHook =
  composeAll
    [ className =? "tweet" --> doRectFloat (RationalRect 0.4 0.4 0.2 0.2)
    ]
