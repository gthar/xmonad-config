{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module MyConfig
    ( mkMain
    , workspaceLog
    , layoutLog
    , taskbar
    ) where

import System.Exit
    ( exitWith
    , ExitCode (ExitSuccess)
    )

import XMonad.Actions.UpdatePointer (updatePointer)

import XMonad.Layout.IndependentScreens (countScreens)

import GHC.IO.Handle.Types (Handle)

import Control.Monad.IO.Class (MonadIO)
import Data.List (intercalate)
import XMonad.Util.NamedWindows (getName)
import XMonad
    ( xmonad
    , (.|.)
    , gets
    )
import XMonad.Core
    ( Layout
    , X
    , ManageHook
    , ScreenDetail
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
    , handleEventHook
    , XConfig (..)
    , withWindowSet
    , whenJust
    , runQuery
    , windowset
    , WorkspaceId
    , WindowSet
    , description
    , io
    )
import XMonad.Config (def)
import Graphics.X11.Types
    ( Window , ButtonMask, Button
    , KeyMask , KeySym
    , mod1Mask , mod4Mask , shiftMask , controlMask
    , xK_Return , xK_Escape , xK_Insert
    , xK_Right , xK_Left , xK_space
    , xK_plus , xK_minus , xK_comma , xK_period
    , button1 , button2 , button3
    , xK_1 , xK_9 , xK_0
    , xK_b , xK_c , xK_e , xK_f , xK_g , xK_h , xK_j , xK_k , xK_l , xK_m
    , xK_n , xK_o , xK_p , xK_q , xK_r , xK_s , xK_t , xK_u , xK_w , xK_x
    , xK_y , xK_z
    , xK_KP_End , xK_KP_Down , xK_KP_Next
    , xK_KP_Add, xK_KP_Subtract, xK_KP_Insert, xK_KP_Enter
    )

import Graphics.X11.ExtraTypes.XF86
    ( xF86XK_AudioMute
    , xF86XK_AudioLowerVolume , xF86XK_AudioRaiseVolume
    , xF86XK_AudioPlay        , xF86XK_AudioStop
    , xF86XK_AudioPrev        , xF86XK_AudioNext
    , xF86XK_RotateWindows    , xF86XK_Launch1
    , xF86XK_MonBrightnessUp  , xF86XK_MonBrightnessDown
    )

import XMonad.Util.Types
    ( Direction2D (U, D, L, R)
    , Direction1D (Next)
    )

import XMonad.Util.Dmenu (menuMapArgs)

import XMonad.Hooks.ManageDocks
    ( docks
    , ToggleStruts (..)
    , avoidStruts
    )

import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)

import XMonad.Layout
    ( Full (..)
    , Mirror (..)
    , IncMasterN (..)
    , Resize (Shrink, Expand)
    , (|||)
    , ChangeLayout (NextLayout)
    )

import XMonad.ManageHook
    ( className
    , (=?)
    , resource
    , composeAll
    , (-->)
    , doFloat
    , doIgnore
    , doShift
    , stringProperty
    )
import XMonad.Operations
    ( windows
    , sendMessage
    , setLayout
    , withFocused
    , screenWorkspace
    , focus
    , mouseMoveWindow
    , mouseResizeWindow
    , restart
    )

import XMonad.StackSet
    ( StackSet (..)
    , RationalRect (..)
    , Workspace (..)
    , shift
    , screen
    , current
    , swapUp
    , swapDown
    , sink
    , view
    , greedyView
    , tag
    , workspace
    , shiftMaster
    , floating
    , stack
    )
import qualified XMonad.StackSet as S

import Data.Monoid (appEndo)
import Data.Ratio ((%))
import Text.Printf (printf)
import qualified Data.Map as M

import XMonad.Util.Run
    ( spawnPipe
    , safeSpawn
    )
import XMonad.Util.Loggers (Logger)
import XMonad.Util.Paste (pasteSelection)

import XMonad.Util.Replace (replace)

import XMonad.Layout.Spacing
    ( toggleScreenSpacingEnabled
    , toggleWindowSpacingEnabled
    , spacingRaw
    , Border (..)
    )
import XMonad.Layout.Grid (Grid (..))

import XMonad.Layout.Tabbed
    ( tabbed
    , shrinkText
    )

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.ResizableTile
    ( ResizableTall (..)
    , MirrorResize (MirrorShrink, MirrorExpand)
    )

import XMonad.Layout.Reflect 
    ( REFLECTX (REFLECTX)
    , REFLECTY (REFLECTY)
    )
import XMonad.Layout.MultiToggle
    ( mkToggle
    , single
    , Toggle (Toggle)
    )

import XMonad.Layout.Named (named)
import XMonad.Layout.PerWorkspace (onWorkspace)

import XMonad.Layout.BoringWindows (boringWindows, focusUp, focusDown)
import XMonad.Layout.Minimize (minimize)
import XMonad.Actions.Minimize
    ( minimizeWindow
    , withLastMinimized
    , maximizeWindowAndFocus
    )
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)

import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.FloatKeys (keysResizeWindow, keysMoveWindow)

import XMonad.Util.WorkspaceCompare

import XMonad.Actions.CycleWS
    ( toggleWS'
    , findWorkspace
    , WSType (WSIs)
    )

import XMonad.Actions.Navigation2D
    ( withNavigation2DConfig
    , Navigation2DConfig
    , centerNavigation
    , singleWindowRect
    , switchLayer
    , defaultTiledNavigation
    , layoutNavigation
    , unmappedWindowRect
    , windowGo
    , windowSwap
    )

import XMonad.Hooks.DynamicLog
    ( dynamicLogWithPP
    , wrap
    , xmobarColor
    , xmobarAction
    , shorten
    , PP
    )
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doRectFloat)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ServerMode (serverModeEventHookCmd')

import XMonad.Util.NamedScratchpad
    ( NamedScratchpad (NS)
    , customFloating
    , namedScratchpadManageHook
    , namedScratchpadAction
    )

import Theme
    ( inactiveColor
    , selectionColor
    , defBg
    , defFg
    , selFg
    , myDecorationTheme
    )

mkMain :: (Handle -> Int -> PP) -> IO ()
mkMain pp = do
    replace
    nscreens <- countScreens
    let
        myScreens = [0 .. nscreens-1]
    xmprocs <- mapM (spawnPipe . xmobarCmd) myScreens
    let
        bars = mapM_ dynamicLogWithPP $ zipWith pp xmprocs myScreens
        sb = addConfMod screenBinder [xK_w, xK_e] [0, 1]
    xmonad $ opts def
        { terminal           = "st"
        , modMask            = mod4Mask
        , borderWidth        = 3
        , normalBorderColor  = defBg
        , focusedBorderColor = selectionColor
        , workspaces         = myWorkspaces
        , keys               = keyComb myKeys sb
        , mouseBindings      = myMouseBindings
        , layoutHook         = myLayoutHook
        , manageHook         = myManageHook
        , logHook            = bars >> fadeInactive >> updatePtr
        , handleEventHook    = serverModeEventHookCmd' myCommands
        , focusFollowsMouse  = True
        , startupHook        = safeSpawn "set_bg" []
        }
    where
        opts = docks . ewmh . withNavigation2DConfig myNav2DConf
        fadeInactive = fadeInactiveLogHook 0.9
        updatePtr = updatePointer (0.9, 0.9) (0, 0)

myNav2DConf :: Navigation2DConfig
myNav2DConf = def
    { defaultTiledNavigation = centerNavigation
    , layoutNavigation   = [("Full", centerNavigation)]
    , unmappedWindowRect = [("Full", singleWindowRect)]
    }

xmobarCmd :: Int -> String
xmobarCmd i = printf "xmobar -x %d %s/%s.hs" i xmobarDir (getXmobarConfig i)
    where
        xmobarDir = "~/.xmobar"
        getXmobarConfig :: Int -> String
        getXmobarConfig 0 = "primary"
        getXmobarConfig _ = "secundary"

myCommands :: X [(String, X ())]
myCommands = return $ workspaceCommands ++ customCommands ++ scratchpadCommands
    where
        customCommands =
            [ ("restart",     restart "xmonad-x86_64-linux" True)
            , ("next_layout", sendMessage NextLayout)
            , ("mixer",       namedScratchpadAction myScratchpads "mixer")
            , ("player",      namedScratchpadAction myScratchpads "player")
            , ("calendar",    namedScratchpadAction myScratchpads "calendar")
            ]
        workspaceCommands = zipWith f myWorkspaces [1..]
            where
                f :: WorkspaceId -> Int -> (String, X ())
                f x i = (("view" ++ show i), onWs greedyView x)
        scratchpadCommands = map f xs
            where
                f x = (x, namedScratchpadAction myScratchpads x)
                xs =
                    [ "mixer"
                    , "player"
                    , "calendar"
                    ]

myWorkspaces :: [String]
myWorkspaces = map show ids
    where ids :: [Int]
          ids = [1..9]

myLayoutHook = fancyThings $ onWorkspace (myWorkspaces !! 8) lws9 allBut9

    where
        tall  = named "tall" $ ResizableTall 1 delta ratio []
        mtall = named "mtall" $ Mirror tall
        tabs  = named "tabs" $ tabbed shrinkText myDecorationTheme
        cols  = named "3cols" $ ThreeColMid 1 delta (1/3)
        full  = named "full" Full
        grid  = named "grid" Grid

        allBut9 = spaces spaced ||| tabs ||| full
            where
                spaced = tall ||| mtall ||| cols
        lws9 = spaces $ mtall ||| grid ||| tall

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

    , NS "telegram"    "telegram-desktop"            (className =? "TelegramDesktop") mngSmallFloat
    , NS "signal"      "signal-desktop --no-sandbox" (className =? "Signal")          mngSmallFloat
    , NS "thunderbird" "thunderbird"                 (className =? "Thunderbird")     mngBiggerFloat
    , NS "mixer"       "pavucontrol"                 (className =? "Pavucontrol")     mngSmallerFloat

    , chromiumApp "whatsapp" "web.whatsapp.com"    mngSmallFloat
    , chromiumApp "hangouts" "hangouts.google.com" mngSmallFloat
    , chromiumApp "calendar" "calendar.google.com" mngBigFloat
    ]

    where
        cmdTerm  = "st"

        termApp name app mng = NS name cmd findIt mng
            where
                cmd = printf fmt cmdTerm name name app
                fmt = "%s -n %s tmux new -A -s %s %s"
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
    , className =? "mpv"           --> doFloat
    , className =? "VirtualBox"    --> doFloat
    , className =? "Pinentry"      --> doFloat
    , className =? "qjackctl"      --> doFloat
    , className =? "Xmessage"      --> doFloat
    , className =? "SuperCollider" --> doFloat
    , role      =? "gimp-dock"     --> doFloat
    , role =? "GtkFileChooserDialog" --> doFloat

    , className =? "Firefox-esr" --> doShift (myWorkspaces !! 2)
    , className =? "firefox" --> doShift (myWorkspaces !! 2)

    -- music
    , resource  =? "wm_player" --> doShift musicWs
    , resource  =? "ignored_cava" --> doIgnore

    --, manageScratchPad
    , namedScratchpadManageHook myScratchpads
    ]
    where
        role     = stringProperty "WM_WINDOW_ROLE"
        musicWs  = (myWorkspaces !! 8)

type Monitor = S.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

getScreen :: Int -> WindowSet -> Monitor
getScreen i = head . filter ((== S i) . S.screen) . S.screens

isScreenFocused :: WindowSet -> Int -> Bool
isScreenFocused x i = focusedScreen == myScreen
    where
        focusedScreen = screen $ current x
        myScreen = screen $ getScreen i x

taskbar :: Int -> Logger
taskbar i = withWindowSet taskFun
    where
        taskFun x = mapM (formatWindows screenFocus) . stack . workspace $ myScreen
            where
                myScreen = getScreen i x
                screenFocus = isScreenFocused x i

        formatWindows screenFocus s@S.Stack {S.focus=focused} = fmt
            where
                ws = S.integrate s
                fmt = intercalate sep <$> mapM (winFmt screenFocus focused) ws

        winFmt focusScreen focusedW x = f <$> getName x
            where
                f = color focusScreen focusWin . wrap " " " " . shorten 80 . show
                focusWin = focusedW == x

        color True  True  = xmobarColor selFg selectionColor
        color False True  = xmobarColor selectionColor ""
        color _     False = xmobarColor inactiveColor ""

        sep = xmobarColor inactiveColor "" "|"

ctlAction :: String -> String -> String
ctlAction cmd = xmobarAction ("xmonadctl " ++ cmd) "1"

getWsState :: (Eq i) => i -> [Workspace i l a] -> i -> Int
getWsState focusedWs xs a
    | a == focusedWs  = 0
    | hasWindows xs a = 1
    | otherwise       = 2
    where
        hasWindows ys = isEmpty . S.stack . (`workspaceByTag` ys)
        isEmpty = not . null . S.integrate'
        workspaceByTag b = head . filter ((== b) . S.tag)

workspaceLog :: (String -> String ) -> Int -> Logger
workspaceLog wsNamer i = withWindowSet $ return . Just . wsFun i
    where
        wsFun j x = fun myWorkspaces
            where
                myWs = tag $ S.workspace $ getScreen j x
                colorer = zipWith (addColor focusedScreen) wsStates
                clickSwitch = zipWith ctlAction cmds
                    where cmds = map ("view"++) myWorkspaces
                fun = (++" ") . intercalate sep . colorer . clickSwitch . map wsNamer
                focusedScreen = isScreenFocused x j
                wsStates = map (getWsState myWs (S.workspaces x)) myWorkspaces

        addColor True  0 = xmobarColor selFg selectionColor
        addColor False 0 = xmobarColor selectionColor ""
        addColor _     1 = xmobarColor defFg ""
        addColor _     2 = xmobarColor inactiveColor ""
        addColor _     _ = xmobarColor defFg ""

        sep = " "

layoutLog :: Int -> Logger
layoutLog i = withWindowSet $ return . Just . fun
    where
        fun = getCmd . getScreen i
            where
                getTxt = description . S.layout . S.workspace
                getCmd = ctlAction "next_layout". wrap " " " " . layoutPrinter . getTxt

        layoutNameCleaner = unwords . filter (not . (`elem` toClean)) . words
            where
                toClean =
                    [ "Simple"
                    , "Simplest"
                    , "Minimize"
                    , "Maximize"
                    , "ImageButtonDeco"
                    , "DefaultDecoration"
                    , "Spacing"
                    , "ReflectX"
                    , "ReflectY"
                    , "0"
                    ]

        layoutPrinter = getLayoutIcon . layoutNameCleaner

getLayoutIcon :: String -> String
getLayoutIcon "empty" = ""
getLayoutIcon x
    | x `elem` icons = printf "<icon=%s/%s.xpm/>" iconsDir x
    | otherwise = x
    where 
        iconsDir = "/home/rilla/.xmonad/icons"
        icons =
            [ "3cols"
            , "float"
            , "full"
            , "grid"
            , "mtall"
            , "tabs"
            , "tall"
            ]


logoutMenu :: MonadIO m => m ()
logoutMenu = dmenuCenterMap actions >>= (`whenJust` id)
    where
        actions = M.fromList
            [ ("\xf08b", io (exitWith ExitSuccess))
            , ("\xf021", safeSpawn "systemctl" ["reboot"])
            , ("\xf011", safeSpawn "systemctl" ["poweroff"])
            , ("\xf023", safeSpawn "slock" [])
            ]

dmenuCenterMap :: MonadIO m => M.Map String a -> m (Maybe a)
dmenuCenterMap = menuMapArgs "dmenu"  args
    where
        args =
            [ "-c"
            , "-fn"
            , printf "%s:size=%d" font fontSize
            ]
        font = "Inconsolata"
        fontSize = 75 :: Int

myKeys :: KeyConfig
myKeys conf@ XConfig {modMask = modm} = M.fromList $
    [ ((modm, xK_Return), safeSpawn (terminal conf) [])

    , ((mod1Mask, xK_Return), namedScratchpadAction myScratchpads "scratchpad")
    , ((mod1Mask, xK_m), namedScratchpadAction myScratchpads "mixer")
    , ((mod1Mask, xK_f), namedScratchpadAction myScratchpads "files")
    , ((mod1Mask, xK_w), namedScratchpadAction myScratchpads "whatsapp")
    , ((mod1Mask, xK_c), namedScratchpadAction myScratchpads "calendar")
    , ((mod1Mask, xK_p), namedScratchpadAction myScratchpads "player")
    , ((mod1Mask, xK_e), namedScratchpadAction myScratchpads "telegram")
    , ((mod1Mask, xK_r), namedScratchpadAction myScratchpads "signal")
    , ((mod1Mask, xK_u), namedScratchpadAction myScratchpads "thunderbird")
    , ((mod1Mask, xK_g), namedScratchpadAction myScratchpads "hangouts")
    , ((mod1Mask, xK_t), namedScratchpadAction myScratchpads "top")

    , ((0, xK_Insert), pasteSelection)

    , ((modm .|. shiftMask, xK_o), restart "obtoxmd" True)
    , ((modm .|. shiftMask, xK_r), restart "xmonad" True)
    --, ((modm .|. shiftMask, xK_Escape), io (exitWith ExitSuccess))
    , ((modm .|. shiftMask, xK_Escape), logoutMenu)

    , ((modm,               xK_n), focusDown)
    , ((modm,               xK_p), focusUp)
    , ((modm .|. shiftMask, xK_n), windows swapDown)
    , ((modm .|. shiftMask, xK_p), windows swapUp)

    , ((modm, xK_plus ), resize M.! "+")
    , ((modm, xK_minus), resize M.! "-")
    , ((modm .|. controlMask, xK_h), resize M.! "L")
    , ((modm .|. controlMask, xK_l), resize M.! "R")
    , ((modm .|. controlMask, xK_j), resize M.! "D")
    , ((modm .|. controlMask, xK_k), resize M.! "U")

    , ((modm, xK_j), windowGo D False)
    , ((modm, xK_k), windowGo U False)
    , ((modm, xK_h), windowGo L False)
    , ((modm, xK_l), windowGo R False)

    , ((modm .|. shiftMask, xK_j), move M.! "D")
    , ((modm .|. shiftMask, xK_k), move M.! "U")
    , ((modm .|. shiftMask, xK_h), move M.! "L")
    , ((modm .|. shiftMask, xK_l), move M.! "R")

    , ((modm .|. controlMask, xK_space), switchLayer)

    , ((0, xF86XK_Launch1      ), safeSpawn "dmenu_run" [])
    , ((0, xF86XK_RotateWindows), safeSpawn "thinkpad-rotate" [])

    , ((modm,                 xK_r   ), safeSpawn "dmenu_run" [])
    , ((modm .|. controlMask, xK_p   ), safeSpawn "passmenu" [])
    , ((modm .|. shiftMask, xK_c     ), kill1)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ layoutHook conf)
    , ((modm,               xK_x     ), sendMessage $ Toggle REFLECTX)
    , ((modm,               xK_y     ), sendMessage $ Toggle REFLECTY)
    , ((modm,               xK_z     ), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_z     ), unminimize)
    , ((modm,               xK_m     ), toggleMax)

    , ((modm,               xK_t     ), withFocused $ windows . sink)
    , ((modm .|. shiftMask, xK_t     ), untile)

    , ((modm,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modm,               xK_period), sendMessage (IncMasterN (-1)))
    , ((modm,               xK_s     ), toggleSpacing)
    , ((modm,               xK_b     ), sendMessage ToggleStruts)

    , ((modm,               xK_Escape), safeSpawn "slock" [])
    --, ((modm .|. shiftMask, xK_Print ), hostSpawn "xfce4-screenshooter" [])
    --, ((modm,               xK_Print ), hostSpawn "xfce4-screenshooter" ["-r"])
    --, ((0,                  xK_Print ), hostSpawn "scrot" [])
    , ((modm,               xK_Right ), nextWS')
    , ((modm,               xK_Left  ), prevWS')
    , ((modm .|. shiftMask, xK_Right ), shiftToNext')
    , ((modm .|. shiftMask, xK_Left  ), shiftToPrev')

    , ((0, xK_KP_End),  safeSpawn "playerctl" ["previous"])
    , ((0, xK_KP_Down), safeSpawn "playerctl" ["play-pause"])
    , ((0, xK_KP_Next), safeSpawn "playerctl" ["next"])

    , ((0, xF86XK_AudioPlay), safeSpawn "playerctl" ["play-pause"])
    , ((0, xF86XK_AudioStop), safeSpawn "playerctl" ["stop"])
    , ((0, xF86XK_AudioPrev), safeSpawn "playerctl" ["previous"])
    , ((0, xF86XK_AudioNext), safeSpawn "playerctl" ["next"])

    , ((0, xF86XK_MonBrightnessUp),   safeSpawn "xbacklight" ["-inc", "10"])
    , ((0, xF86XK_MonBrightnessDown), safeSpawn "xbacklight" ["-dec", "10"])

    , ((0, xK_KP_Subtract), safeSpawn "pactl" ["set-sink-volume", "0", "-5%"])
    , ((0, xK_KP_Add),      safeSpawn "pactl" ["set-sink-volume", "0", "+5%"])
    , ((0, xK_KP_Enter),    safeSpawn "pactl" ["set-sink-volume", "0", "100%"])
    , ((0, xK_KP_Insert),   safeSpawn "pactl" ["set-sink-mute",   "0", "toggle"])

    , ((0, xF86XK_AudioLowerVolume), safeSpawn "pactl" ["set-sink-volume", "0", "-5%"])
    , ((0, xF86XK_AudioRaiseVolume), safeSpawn "pactl" ["set-sink-volume", "0", "+5%"])
    , ((0, xF86XK_AudioMute),        safeSpawn "pactl" ["set-sink-mute",   "0", "toggle"])

    , ((modm, xK_q), safeSpawn "query_mopidy" [])
    , ((modm, xK_0), toggleWS' ["NSP"])
    ]
    ++ wsBinder modm [xK_1 .. xK_9] (workspaces conf)

    where
        toggleSpacing = toggleWindowSpacingEnabled >> toggleScreenSpacingEnabled
        toggleMax = withFocused (sendMessage . maximizeRestore)
        unminimize = withLastMinimized maximizeWindowAndFocus

        untile = withFocused rectFloatFocused
            where
                rectFloatFocused focused = action focused >>= windows
                action = fmap appEndo . doIt
                doIt = runQuery $ doRectFloat rect
                rect = RationalRect 0.05 0.05 0.9 0.9

        floatResize = M.fromList
            [ ("L", (-n, 0))
            , ("R", (n, 0))
            , ("D", (0, n))
            , ("U", (0, -n))
            ]
        tillingResize = M.fromList
            [ ("L", sendMessage Shrink)
            , ("R", sendMessage Expand)
            , ("D", sendMessage MirrorShrink)
            , ("U", sendMessage MirrorExpand)
            ]
        tillingMove = M.fromList
            [ ("L", L)
            , ("R", R)
            , ("D", D)
            , ("U", U)
            ]
        incDec = M.fromList
            [ ("+", (n,n))
            , ("-", (-n,-n))
            ]
        n = 10

        resize = M.union lrdu chSize
            where
                flt = fmap (\x -> keysResizeWindow x (0,0)) floatResize
                lrdu = M.intersectionWith onFloat flt tillingResize
                chSize = fmap (g . f) incDec
                    where
                        f x = keysResizeWindow x (1%2,1%2)
                        g x = onFloat x $ return ()

        move = M.intersectionWith onFloat flt tilling
            where
                flt = fmap keysMoveWindow floatResize
                tilling = fmap (`windowSwap` False) tillingMove

        onFloat a b = withFocused $ ifFloat a (const b)
            where
                ifFloat x y w = isFloat w >>= picker x y w

                picker x _ w True = x w
                picker _ y w False = y w

                isFloat :: Window -> X Bool
                isFloat w = M.member w . floating <$> gets windowset

        prevWS' = wsCycler greedyView (-1)
        nextWS' = wsCycler greedyView 1
        shiftToPrev' = wsCycler shift (-1)
        shiftToNext' = wsCycler shift 1

        wsCycler f d = wsBy' d >>= windows . f
            where
                wsBy' = findWorkspace getSortByIndex Next anyExcept
                anyExcept = let fun (Workspace wsId _ _) = wsId `notElem` skips
                            in WSIs $ return fun
                skips = ["NSP"]


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


type WsAction = WorkspaceId -> WindowSet -> WindowSet
type OnHandler a = WsAction -> a -> X()
type KeyBind = ((ButtonMask, KeySym), X ())
type KeyBinder a = KeyMask -> [KeySym] -> [a] -> [KeyBind]
type KeyConfig = XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())

onWs :: OnHandler WorkspaceId
onWs f i = windows $ f i

onScreen :: OnHandler ScreenId
onScreen f i = screenWorkspace i >>= (`whenJust` onWs f)

-- mod-X to move focus
-- mod-shift-X to shift window
-- mod-ctrl-X to copy window
binder :: KeyMask -> OnHandler a -> [WsAction] -> [KeySym] -> [a] -> [KeyBind]
binder modm onX actions ks is = concat $ zipWith zipBind ks is
    where
        zipBind k i = zipWith (bind k i) mods actions
        bind k i m f = ((modm .|. m, k), onX f i)
        mods = [0, shiftMask, controlMask]

wsBinder :: KeyBinder WorkspaceId
wsBinder modm = binder modm onWs actions
    where
        actions = [greedyView, shift, copy]

screenBinder :: KeyBinder ScreenId
screenBinder modm = binder modm onScreen actions
    where
        actions = [view, shift, copy]

addConfMod :: KeyBinder a -> [KeySym] -> [a] -> KeyConfig
addConfMod f is ks XConfig {modMask = modm} = M.fromList $ f modm is ks

keyComb :: KeyConfig -> KeyConfig -> KeyConfig
keyComb f g conf =
    let
        a = f conf
        b = g conf
    in
        M.union a b
