{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module MyConfig (mkMain) where

import XMonad.Actions.UpdatePointer (updatePointer)

import XMonad.Layout.IndependentScreens
    ( countScreens
    , withScreens
    )

import XMonad (xmonad)

import XMonad.Core
    ( Layout
    , X
    , ManageHook
    , ScreenId (S)
    , terminal
    , modMask
    , borderWidth
    , normalBorderColor
    , focusedBorderColor
    , workspaces
    , keys
    , mouseBindings
    , layoutHook
    , manageHook
    , XConfig (..)
    )

import XMonad.Config (def)

import Graphics.X11.Types
    ( Window
    , Button
    , KeyMask
    , mod4Mask
    , button1
    , button2
    , button3
    )

import XMonad.Hooks.ManageDocks
    ( docks
    , avoidStruts
    )

-- import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)

import XMonad.Layout
    ( Full (..)
    , Mirror (..)
    , (|||)
    )

import XMonad.ManageHook
    ( className
    , (=?)
    , resource
    , composeAll
    , (-->)
    , doFloat
    , stringProperty
    )

import XMonad.Operations
    ( windows
    , focus
    , mouseMoveWindow
    , mouseResizeWindow
    )

import XMonad.StackSet
    ( RationalRect (..)
    , shiftMaster
    )

import Text.Printf (printf)
import qualified Data.Map as M

import XMonad.Util.Run
    ( spawnPipe
    , safeSpawn
    )

import XMonad.Util.Replace (replace)

import XMonad.Layout.Spacing
    ( spacingRaw
    , Border (..)
    )

import XMonad.Layout.Tabbed
    ( tabbed
    , shrinkText
    )

import XMonad.Layout.NoBorders (smartBorders)

import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))

import XMonad.Layout.ResizableTile (ResizableTall (..))

import XMonad.Layout.Reflect 
    ( REFLECTX (REFLECTX)
    , REFLECTY (REFLECTY)
    )

import XMonad.Layout.MultiToggle
    ( mkToggle
    , single
    )

import XMonad.Layout.Named (named)

import XMonad.Layout.BoringWindows (boringWindows)

import XMonad.Layout.Minimize (minimize)

import XMonad.Layout.Maximize (maximizeWithPadding)

import XMonad.Actions.Navigation2D
    ( withNavigation2DConfig
    , Navigation2DConfig
    , centerNavigation
    , singleWindowRect
    , defaultTiledNavigation
    , layoutNavigation
    , unmappedWindowRect
    )

import XMonad.Hooks.DynamicLog (dynamicLogWithPP)

import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops (ewmh)

import XMonad.Util.NamedScratchpad
    ( NamedScratchpad (NS)
    , customFloating
    , namedScratchpadManageHook
    )

import HostConfig
    ( HostConfig
    , fontSize
    , completeTaskbar
    , showLayout
    )

import Theme
    ( myDecorationTheme
    , defBg
    , defFg
    , inactiveBorder
    , selectionColor
    , fontName
    )

import MyPP (mkPP)
import Keybinds (keybinds)

mkMain :: HostConfig -> IO ()
mkMain hostConfig = do
    replace
    nscreens <- countScreens
    let
        myScreens = [0 .. nscreens-1]
        wsLs = withScreens (S nscreens) myWorkspaces
    xmprocs <- mapM (spawnPipe . xmobarCmd (fontSize hostConfig)) myScreens
    let
        pp = mkPP (completeTaskbar hostConfig) (showLayout hostConfig) wsLs
        bars = mapM_ dynamicLogWithPP $ zipWith pp xmprocs myScreens
    xmonad $ opts def
        { terminal           = "alacritty"
        , modMask            = mod4Mask
        , borderWidth        = 4
        , normalBorderColor  = inactiveBorder
        , focusedBorderColor = selectionColor
        , workspaces         = wsLs
        , keys               = keybinds (fontSize hostConfig) myScratchpads
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayoutHook $ fontSize hostConfig
        , manageHook         = myManageHook
        --, logHook            = bars >> fadeInactive >> updatePtr
        , logHook            = bars >> updatePtr
        , focusFollowsMouse  = True
        , startupHook        = safeSpawn "set_bg" []
        }
    where
        opts = docks . ewmh . withNavigation2DConfig myNav2DConf
        --fadeInactive = fadeInactiveLogHook 0.9
        updatePtr = updatePointer (0.9, 0.9) (0, 0)

myNav2DConf :: Navigation2DConfig
myNav2DConf = def
    { defaultTiledNavigation = centerNavigation
    , layoutNavigation   = [("Full", centerNavigation)]
    , unmappedWindowRect = [("Full", singleWindowRect)]
    }

xmobarCmd :: Int -> Int -> String
xmobarCmd fSize i = unwords ("xmobar":args)
    where
        args =
            [ printf "--font='xft:%s:style=Regular:size=%d'" fontName fSize
            , printf "--bgcolor='%s'" defBg
            , printf "--fgcolor='%s'" defFg
            , printf "--screen='%d'" i
            , printf "%s/%s.hs" xmobarDir (getXmobarConfig i)
            ]
        xmobarDir = "~/.xmobar"
        getXmobarConfig :: Int -> String
        getXmobarConfig 0 = "primary"
        getXmobarConfig _ = "secundary"


myWorkspaces :: [String]
myWorkspaces = map show ids
    where ids :: [Int]
          ids = [1..9]

myLayoutHook fSize = fancyThings layouts
    where
        layouts = spaces spaced ||| tabs ||| full
            where
                spaced = tall ||| mtall ||| cols

        tall  = named "tall" $ ResizableTall 1 delta ratio []
        mtall = named "mtall" $ Mirror tall
        tabs  = named "tabs" $ tabbed shrinkText (myDecorationTheme fSize)
        cols  = named "3cols" $ ThreeColMid 1 delta (1/3)
        full  = named "full" Full

        spaces = spacingRaw False b True b True
            where
                b = Border defSpacing defSpacing defSpacing defSpacing
                defSpacing = 5

        fancyThings = avoidStruts . reflector .smartBorders . maxMin . boringWindows
            where
                maxMin = maximizeWithPadding 0 . minimize
                reflector = reflectx . reflecty
                reflectx = mkToggle (single REFLECTX)
                reflecty = mkToggle (single REFLECTY)
        ratio = 1/2
        delta = 3/100

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ termApp "scratchpad" "zsh"          mngTopScratch
    , termApp "top"        "top"          mngBigFloat
    --, termApp "mixer"      "pulsemixer"   mngSmallerFloat
    , termApp "player"     "ncmpcpp"      mngBiggerFloat
    , termApp "bookmarks"  "oil -p"       mngBiggerFloat

    , NS "telegram"    "telegram-desktop"            (className =? "TelegramDesktop") mngSmallFloat
    , NS "signal"      "signal-desktop --no-sandbox" (className =? "Signal")          mngSmallFloat
    , NS "thunderbird" "thunderbird"                 (className =? "Thunderbird")     mngBiggerFloat
    , NS "mixer"       "pavucontrol"                 (className =? "Pavucontrol")     mngSmallerFloat

    , chromiumApp "whatsapp" "web.whatsapp.com"    mngSmallFloat
    , chromiumApp "hangouts" "hangouts.google.com" mngSmallFloat
    , chromiumApp "calendar" "calendar.google.com" mngBigFloat
    ]

    where
        termApp name app mng = NS name cmd findIt mng
            where
                cmd = printf fmt name name app
                fmt = "alacritty --class %s --command tmux new -A -s %s %s"
                --fmt = "st -n %s tmux new -A -s %s %s"

                findIt = resource =? name

        chromiumApp name url mng = NS name cmd findIt mng
            where
                cmd = printf "chromium --app=https://%s" url
                findIt = resource =? url

        mngSmallerFloat = centeredFloat 0.6
        mngSmallFloat = centeredFloat 0.7
        mngBigFloat = centeredFloat 0.8
        mngBiggerFloat = centeredFloat 0.9
        centeredFloat s = customFloating $ RationalRect p p s s
            where
                p = (1-s) / 2

        mngTopScratch = customFloating $ RationalRect l t w h
            where
                h = 0.3    -- height, 30%
                w = 1      -- width, 100%
                t = 0      -- distance from top edge, 0%
                l = 1 - w  -- distance from left edge, 0%

myManageHook :: ManageHook
myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "MPlayer"       --> doFloat
    --, className =? "mpv"           --> doFloat
    , className =? "VirtualBox"    --> doFloat
    , className =? "Pinentry"      --> doFloat
    , className =? "qjackctl"      --> doFloat
    , className =? "Xmessage"      --> doFloat
    , className =? "SuperCollider" --> doFloat
    , role      =? "gimp-dock"     --> doFloat
    , role =? "GtkFileChooserDialog" --> doFloat

    --, manageScratchPad
    , namedScratchpadManageHook myScratchpads
    ]
    where
        role     = stringProperty "WM_WINDOW_ROLE"

myMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {modMask = modm} = M.fromList bindings
    where
        bindings =
            [ ((modm, button1), move)
            , ((modm, button2), toMaster)
            , ((modm, button3), resize)
            ]
        move = mouseDo mouseMoveWindow
        resize = mouseDo mouseResizeWindow
        toMaster = mouseDo return
        mouseDo f w = focus w >> f w >> windows shiftMaster
