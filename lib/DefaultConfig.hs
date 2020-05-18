module DefaultConfig
    ( mkPP
    , wsNamer
    , defaultPP
    ) where

import MyConfig
    ( workspaceLog
    , layoutLog
    , taskbar
    )

import Theme
    ( inactiveColor
    , urgentColor
    , selFg
    , selectionColor
    )

import GHC.IO.Handle.Types (Handle)
import XMonad.Hooks.DynamicLog (PP)

import XMonad.Hooks.DynamicLog
    ( ppCurrent
    , ppOutput
    , ppExtras
    , ppVisible
    , ppHidden
    , ppHiddenNoWindows
    , ppUrgent
    , ppOrder
    , ppTitle
    , ppSep
    , ppLayout
    , xmobarColor
    , wrap
    , shorten
    )

import XMonad.Config (def)

import XMonad.Util.Run (hPutStrLn)


mkPP :: (String -> String) -> Bool -> Handle -> Int -> PP
mkPP workspaceNamer complete bar screen = common
    { ppOutput = hPutStrLn bar
    , ppExtras = extras complete
    }
    where
        common = def
            { ppCurrent         = const ""
            , ppVisible         = const ""
            , ppHidden          = const ""
            , ppHiddenNoWindows = const ""
            , ppUrgent          = xmobarColor urgentColor ""
            , ppOrder           = order complete
            , ppTitle           = title complete
            , ppSep             = xmobarColor inactiveColor "" "|"
            , ppLayout          = const ""
            }

        extras True  =
            [ workspaceLog workspaceNamer screen
            , layoutLog screen
            , taskbar screen
            ]
        extras False =
            [ workspaceLog workspaceNamer screen
            , layoutLog screen
            ]

        order True  (_:_:_:xs) = xs
        order False (_:_:t:ws:l:_) = [ws, l, t]
        order _ _ = []

        title True  = const ""
        title False = wrap " " "" . xmobarColor selFg selectionColor . wrap " " " " . shorten 80

wsNamer :: String -> String
wsNamer "NSP" = ""
wsNamer x = x

defaultPP :: Handle -> Int -> PP
defaultPP = mkPP wsNamer True
