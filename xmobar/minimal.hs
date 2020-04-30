-- todo: network monitoring
Config
    { font = "xft:Inconsolata for Powerline:style=Regular:size=14,FontAwesome:size=14"
    , additionalFonts = ["xft:FontAwesome:size=14"]
    , position = TopW L 100
    , border = BottomB 0
    , bgColor = "#282828"
    , borderColor = "#282828"
    , fgColor = "#ebdbb2"
    , lowerOnStart = True
    , hideOnStart = False
    , persistent = True
    , allDesktops = True
    , sepChar = "%"
    , alignSep = "}{"
    , template = "%UnsafeStdinReader% }{ %date% "
    , commands =
        [ Run Date "%a %b %_d <fc=#a89974>|</fc> %H:%M" "date" 10
        , Run UnsafeStdinReader
        ]
    }
