module Theme
    ( myBorderWidth
    , inactiveColor
    , inactiveBorder
    , selectionColor
    , urgentColor
    , defBg
    , defFg
    , selFg
    , selBg
    , myDecorationTheme
    , fontName
    ) where

import Text.Printf (printf)

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

inactiveBorder :: String
inactiveBorder = "#504945"

inactiveColor :: String
inactiveColor  = "#a89974"

selectionColor :: String
selectionColor = "#458588"

urgentColor :: String
urgentColor = "#98971a"

defBg :: String
defBg = "#282828"

defFg :: String
defFg = "#ebdbb2"

selFg :: String
selFg = "#fbf1c7"

selBg :: String
selBg = selectionColor

fontName :: String
fontName = "Tex Gyre Heros"

myDecorationTheme :: Int -> XMonad.Layout.Decoration.Theme
myDecorationTheme fontSize = def
    { T.activeColor         = selBg
    , T.activeBorderColor   = inactiveColor
    , T.activeTextColor     = selFg

    , T.inactiveColor       = defBg
    , T.inactiveBorderColor = inactiveBorder
    , T.inactiveTextColor   = defFg

    , T.urgentColor         = urgentColor
    , T.urgentTextColor     = urgentColor
    , T.fontName            = printf "xft:%s:size=%d" fontName fontSize
    }
