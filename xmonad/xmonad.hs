--
-- Imports
-- =======================================
import System.IO
import Data.Char
import XMonad
import qualified XMonad.StackSet as W

-- Used for isFullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile

import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows

import XMonad.Actions.CycleWS

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

myWorkspaces = ["web","term"] ++ map show [3..9]

myLayout = tiled ||| Mirror tiledSpace ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = spacing 5 $ ResizableTall nmaster delta ratio []
    tiledSpace = spacing 60 $ ResizableTall nmaster delta ratio []

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = toRational (2/(1 + sqrt 5 :: Double))

    -- Percent of screen to increment by when resizing panes
    delta = 5/100

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
        , layoutHook = smartBorders $ avoidStruts  $ myLayout
        , workspaces = myWorkspaces
        , startupHook   = setWMName "LG3D"
        -- Gets piped to xmobar
        , logHook = dynamicLogWithPP xmobarPP
          { ppOutput    = hPutStrLn xmproc
            , ppCurrent = xmobarColor "#FEE799" "#1d1f21" . pad
            , ppVisible = pad
            , ppOrder   = \(ws:_:_:_) -> [ws]
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
