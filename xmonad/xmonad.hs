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
import XMonad.Layout.ToggleLayouts

import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ScreenCorners

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

import XMonad.Prompt
import XMonad.Prompt.Window

--
-- Tomorrow Night Colors
-- ===

colorBackground = "#1d1f21"
colorCurrent    = "#282a2e"
colorSelection  = "#373b41"
colorForeground = "#c5c8c6"
colorComment    = "#969896"
colorRed        = "#cc6666"
colorOrange     = "#de935f"
colorYellow     = "#f0c674"
colorGreen      = "#b5bd68"
colorAqua       = "#8abeb7"
colorBlue       = "#81a2be"
colorPurple     = "#b294bb"

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
    , className =? "Svkbd" --> doFloat
    , className =? "Google-chrome" --> doShift "web"
    , isFullscreen --> doFullFloat
    ]

myWorkspaces = ["web","term"] ++ map show [3..9]

myLayout = avoidStruts
            $ toggleLayouts tiledSpace
            $ smartBorders $ basicRotate
  where
    basicRotate =  Full ||| fullTiled ||| Mirror fullTiled

    -- tiled = spacing 5 $ ResizableTall nmaster delta ratio []
    tiledSpace = spacing 60 $ ResizableTall nmaster delta ratio []
    fullTiled = ResizableTall nmaster delta ratio []

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = toRational (2/(1 + sqrt 5 :: Double))

    -- Percent of screen to increment by when resizing panes
    delta = 5/100

myStartupHook = do
  setWMName "LG3D"
  addScreenCorner SCUpperRight $ goToSelected defaultGSConfig
  addScreenCorner SCLowerRight $ sendMessage ToggleLayout

myEventHook = fullscreenEventHook
  <+> screenCornerEventHook
  <+> docksEventHook

--
-- Main Layout
-- =======================================
main = do
    xmproc <- spawnPipe "xmobar /home/tyler/.xmobarrc"
    xmonad $ withUrgencyHook LibNotifyUrgencyHook $ defaultConfig
      { modMask = mod4Mask
        , normalBorderColor = colorBackground
        , focusedBorderColor = colorSelection
        , terminal = "urxvt"
        , manageHook = manageDocks
                        <+> myManageHook
                        <+> manageHook defaultConfig
        , handleEventHook = myEventHook
        , layoutHook = myLayout
        , workspaces = myWorkspaces
        , startupHook   = myStartupHook
        -- Gets piped to xmobar
        , logHook = dynamicLogWithPP xmobarPP
          { ppOutput    = hPutStrLn xmproc
            , ppCurrent = xmobarColor colorGreen colorBackground . pad
            , ppVisible = pad
            , ppOrder   = \(ws:_:_:_) -> [ws]
          }
     }
     `additionalKeysP`
     [ ("M-p", spawn "x=$(yeganesh -x -- -i -fn Verdana-12) && exec $x")
     , ("M-b", sendMessage ToggleStruts)
     , ("M1-<Tab>", prevScreen)
     , ("M1-S-<Tab>",  nextScreen)
     , ("M-/", windowPromptGoto defaultXPConfig)
     , ("M-q", spawn "xmonad --recompile && xmonad --restart")
     , ("M-<F11>",  nextWS)
     , ("M-<F12>",  prevWS)
     ]
     `additionalMouseBindings`
     [((0, 6), (\_ -> moveTo Next NonEmptyWS))
     ,((0, 7), (\_ -> moveTo Prev NonEmptyWS))
     ,((mod4Mask, 5), (\_ -> moveTo Prev NonEmptyWS))
     ,((mod4Mask, 4), (\_ -> moveTo Next NonEmptyWS))
     ]
