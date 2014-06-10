--
-- Imports
-- =======================================
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Used for isFullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)

import XMonad.Util.Run

import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Actions.CycleWS

import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows

import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.SetWMName

--
-- Window WM_URGENT
-- ==================
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

--
-- Window Rules
-- =======================================
myManageHook = composeAll
    [ className =? "dmenu"     --> doFloat
    , className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , isFullscreen --> doFullFloat
    ]

myWorkspaces = ["1:web","2:term"] ++ map show [3..9]

--
-- Main Layout
-- =======================================
main = do
    xmproc <- spawnPipe "xmobar /home/tyler/.xmobarrc"
    xmonad $ withUrgencyHook LibNotifyUrgencyHook $ defaultConfig
      { modMask = mod4Mask
        , normalBorderColor = "#073642"
        , focusedBorderColor = "#268bd2"
        , terminal = "urxvt"
        , manageHook = manageDocks
                        <+> myManageHook
                        <+> manageHook defaultConfig
        , handleEventHook = fullscreenEventHook
        , layoutHook = smartBorders $ avoidStruts  $  layoutHook defaultConfig
        , workspaces = myWorkspaces
        , startupHook   = setWMName "LG3D"
        -- Gets piped to xmobar
        , logHook = dynamicLogWithPP xmobarPP
          { ppOutput    = hPutStrLn xmproc
            , ppCurrent = xmobarColor "#FEE799" "#35383C" . pad
            , ppVisible = pad
            , ppTitle   = xmobarColor "#859900" "" . shorten 50
            , ppOrder   = \(ws:_:t:_) -> [ws,t]
          }
     }
     `additionalKeys`
     [ ((mod1Mask, xK_Tab), prevScreen)
     , ((mod1Mask .|. shiftMask, xK_Tab),  nextScreen)
     ]
     `additionalMouseBindings`
     [((mod4Mask , 6), (\_ -> moveTo Next NonEmptyWS))
     ,((mod4Mask , 7), (\_ -> moveTo Prev NonEmptyWS))
     ,((mod4Mask , 5), (\_ -> moveTo Prev NonEmptyWS))
     ,((mod4Mask , 4), (\_ -> moveTo Next NonEmptyWS))
     ]
