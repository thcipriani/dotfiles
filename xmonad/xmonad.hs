--
-- Imports
-- =======================================
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Used for isFullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)

-- Used for xmobar and notify-send
import XMonad.Util.Run

import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO
import XMonad.Actions.CycleWS

import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows

import XMonad.Hooks.EwmhDesktops


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
    [ className =? "Gimp"      --> doFloat
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

        -- Gets piped to xmobar
        , logHook = dynamicLogWithPP xmobarPP
          {
            ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "#859900" "" . shorten 50
          }
     }
     `additionalKeys`
     [ ((mod1Mask, xK_Tab), prevScreen)
     , ((mod1Mask .|. shiftMask, xK_Tab),  nextScreen)
     ]
