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
import XMonad.Layout.Tabbed

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
import XMonad.Util.NamedScratchpad

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

import XMonad.Prompt
import XMonad.Prompt.Window

-- crude sticky pane
import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

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
    [ className =? "dmenu"         --> doFloat
    , className =? "Gimp"          --> doFloat
    , className =? "Vncviewer"     --> doFloat
    , className =? "Svkbd"         --> doFloat
    , className =? "Google-chrome" --> doShift "web"
    , className =? "Firefox"       --> doShift "web"
    , isFullscreen                 --> doFullFloat
    , namedScratchpadManageHook scratchpads
    ]

scratchpads =
  [
    NS "scratch" "urxvt --title scratch" (title =? "scratch")
      (customFloating $ W.RationalRect 0.6 0.6 0.4 0.4)
  ]

myWorkspaces = ["web","term"] ++ map show [3..9]

myTabConfig = defaultTheme { inactiveBorderColor = colorBackground
                           , activeBorderColor = colorGreen
                           , activeColor = colorCurrent
                           , inactiveColor = colorBackground
                           , inactiveTextColor = colorComment
                           , activeTextColor = colorForeground
                           }

myLayout = avoidStruts
            $ toggleLayouts tiledSpace
            $ smartBorders $ basicRotate
  where
    basicRotate =  Full ||| tabbed shrinkText myTabConfig ||| fullTiled ||| Mirror fullTiled

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

myTerminal = "urxvt"

--
-- Main Layout
-- =======================================
main = do
    xmproc <- spawnPipe "xmobar ~/.xmobarrc"
    yeganesh_cmd <- lookupEnv "YEGANESH_CMD"
    let yeganesh = fromMaybe "yeganesh -x -- -i -fn '-xos4-*-medium-r-*-*-16-*'" yeganesh_cmd
    xmonad $ withUrgencyHook LibNotifyUrgencyHook $ defaultConfig
      { modMask = mod4Mask
        , normalBorderColor = colorBackground
        , focusedBorderColor = colorSelection
        , terminal = myTerminal
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
     [ ("M-p", spawn $ "x=$(" ++ yeganesh ++ ") && exec $x")
     , ("M-S-p", namedScratchpadAction scratchpads "scratch")
     , ("M-v", spawn "urxvt -e alsamixer -c 1")
     , ("M-S-v", spawn "xdotool click 2")
     , ("M-l", spawn "slock")
     , ("M-b", sendMessage ToggleStruts)
     , ("M1-<Tab>", prevScreen)
     , ("M1-S-<Tab>",  nextScreen)
     , ("M-/", windowPromptGoto defaultXPConfig)
     , ("M-q", spawn "xmonad --recompile && xmonad --restart")
     , ("M-<F11>",  nextWS)
     , ("M-<F12>",  prevWS)
     -- , ("M-S-<Space>", layoutScreens 2 (TwoPane 0.33 0.66))
     -- , ("M-S-C-<Space>", rescreen)
     , ("<XF86KbdBrightnessUp>", spawn "keyboard-bl up")
     , ("<XF86KbdBrightnessDown>", spawn "keyboard-bl down")
     , ("<XF86AudioRaiseVolume>", spawn "volume up")
     , ("<XF86AudioLowerVolume>", spawn "volume down")
     , ("<XF86AudioMute>", spawn "volume mute")
     ]
