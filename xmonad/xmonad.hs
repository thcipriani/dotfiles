--
-- Imports
-- =======================================
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Used for isFullscreen
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat) 

-- Used for xmobar
import XMonad.Util.Run(spawnPipe)

import XMonad.Util.EZConfig(additionalKeys)
import System.IO

--
-- Window Rules
-- =======================================
myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , isFullscreen --> doFullFloat
    ]

--
-- Main Layout
-- =======================================
main = do
    xmproc <- spawnPipe "xmobar /home/tyler/.xmobarrc"
    xmonad $ defaultConfig
      { 
        terminal = "urxvt"
        , manageHook = manageDocks 
                        <+> myManageHook 
                        <+> manageHook defaultConfig
        , layoutHook = smartBorders $ avoidStruts  $  layoutHook defaultConfig
        , workspaces = ["1:web","2:term"] ++ map show [3..9]

        -- Gets piped to xmobar
        , logHook = dynamicLogWithPP xmobarPP
          { 
            ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "#859900" "" . shorten 50
          }
     }
