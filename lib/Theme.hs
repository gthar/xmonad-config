module Theme
    ( myBorderWidth
    , inactiveColor
    , selectionColor
    , urgentColor
    , defBg
    , defFg
    , selFg
    , selBg
    , myDecorationTheme
    ) where

import XMonad.Config (def)
import qualified XMonad.Layout.Tabbed as T
    ( activeColor
    , inactiveColor
    , activeBorderColor
    , inactiveBorderColor
    , urgentColor
    , activeTextColor
    , inactiveTextColor
    , urgentTextColor
    , fontName
    )

import XMonad.Layout.Decoration (Theme)

myBorderWidth :: Int
myBorderWidth = 3

-- https://github.com/morhetz/gruvbox

inactiveColor :: String
inactiveColor  = "#a89974"

selectionColor :: String
--selectionColor = "#cc241d"
selectionColor = "#458588"

urgentColor :: String
urgentColor = "#98971a" :: String

defBg :: String
defBg = "#282828"

defFg :: String
defFg = "#ebdbb2"

selFg :: String
selFg = "#fbf1c7"

selBg :: String
selBg = selectionColor

font :: String
font = "xft:Inconsolata:size=12"

myDecorationTheme :: XMonad.Layout.Decoration.Theme
myDecorationTheme = def
    { T.activeColor         = selBg
    , T.activeBorderColor   = inactiveColor
    , T.activeTextColor     = selFg

    , T.inactiveColor       = defBg
    , T.inactiveBorderColor = inactiveColor
    , T.inactiveTextColor   = defFg

    , T.urgentColor         = urgentColor
    , T.urgentTextColor     = urgentColor
    , T.fontName            = font
    }
