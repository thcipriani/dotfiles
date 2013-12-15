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

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , isFullscreen --> doFullFloat
    ]

main = do
    xmproc <- spawnPipe "xmobar /home/tyler/.xmobarrc"
    xmonad $ defaultConfig { 
        terminal = "urxvt"
        , manageHook = manageDocks <+> myManageHook 
                        <+> manageHook defaultConfig
        , layoutHook = smartBorders $ avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP { 
            ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
        }
