-- todo: network monitoring
Config
    { font = "xft:Inconsolata for Powerline:style=Regular:size=12,FontAwesome:size=12"
    , additionalFonts = ["xft:FontAwesome:size=12"]
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
    , template = "%UnsafeStdinReader% } <action=`xmonadctl calendar`>%date%</action> {%sesame%%posture%%mopidy%%vol%%bat%%tray%"
    , commands =
        [ Run Date "%a %b %_d <fc=#a89974>|</fc> %H:%M" "date" 10
        , Run UnsafeStdinReader
        , Run Com "query_battery" [] "bat" 10
        , Run Com "query_sesame" [] "sesame" 30
        , Run CommandReader "monitor_mopidy" "mopidy"
        , Run CommandReader "monitor_volume" "vol"
        , Run CommandReader "posture_reminder" "posture"
        , Run Com "tray_wrapper" ["4"] "tray" 0
        ]
    }
