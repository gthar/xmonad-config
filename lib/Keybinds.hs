module Keybinds (keybinds) where

import System.Exit
    ( exitWith
    , ExitCode (ExitSuccess)
    )

import XMonad.Layout.IndependentScreens
    ( workspaces'
    , onCurrentScreen
    )

import XMonad
    ( (.|.)
    , gets
    )
import XMonad.Core
    ( Layout
    , X
    , terminal
    , modMask
    , layoutHook
    , XConfig (..)
    , whenJust
    , runQuery
    , windowset
    , io
    )

import Graphics.X11.Types
    ( Window , ButtonMask , KeySym
    , mod1Mask , shiftMask , controlMask
    , xK_Return , xK_Escape , xK_Insert
    , xK_Right , xK_Left , xK_space
    , xK_plus , xK_minus , xK_comma , xK_period
    , xK_1 , xK_9 , xK_0
    , xK_b , xK_c , xK_e , xK_f , xK_g , xK_h , xK_j , xK_k , xK_l , xK_m
    , xK_n , xK_o , xK_p , xK_r , xK_s , xK_t , xK_u , xK_w , xK_x
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

import XMonad.Hooks.ManageDocks (ToggleStruts (..))

import XMonad.Layout
    ( IncMasterN (..)
    , Resize (Shrink, Expand)
    , ChangeLayout (NextLayout)
    )

import XMonad.Operations
    ( windows
    , sendMessage
    , setLayout
    , withFocused
    , screenWorkspace
    , restart
    )

import XMonad.StackSet
    ( StackSet (..)
    , RationalRect (..)
    , Workspace (..)
    , shift
    , greedyView
    , swapUp
    , swapDown
    , sink
    , view
    , floating
    )

import Data.Monoid (appEndo)
import Data.Ratio ((%))
import qualified Data.Map as M

import XMonad.Util.Run (safeSpawn)

import XMonad.Util.Paste (pasteSelection)

import XMonad.Layout.Spacing
    ( toggleScreenSpacingEnabled
    , toggleWindowSpacingEnabled
    )

import XMonad.Layout.ResizableTile (MirrorResize (MirrorShrink, MirrorExpand))

import XMonad.Layout.Reflect
    ( REFLECTX (REFLECTX)
    , REFLECTY (REFLECTY)
    )
import XMonad.Layout.MultiToggle (Toggle (Toggle))

import XMonad.Layout.BoringWindows (focusUp, focusDown)
import XMonad.Actions.Minimize
    ( minimizeWindow
    , withLastMinimized
    , maximizeWindowAndFocus
    )
import XMonad.Layout.Maximize (maximizeRestore)

import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.FloatKeys (keysResizeWindow, keysMoveWindow)

import XMonad.Util.WorkspaceCompare

import XMonad.Actions.CycleWS
    ( toggleWS'
    , findWorkspace
    , WSType (WSIs)
    )

import XMonad.Actions.Navigation2D
    ( switchLayer
    , windowGo
    , windowSwap
    )

import XMonad.Hooks.ManageHelpers (doRectFloat)

import XMonad.Util.NamedScratchpad
    ( NamedScratchpads
    , namedScratchpadAction
    )

import HostConfig
    ( HostConfig
    , guiMenu
    )

type KeyConfig = XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())

keybinds :: HostConfig -> NamedScratchpads -> KeyConfig
keybinds hostConfig scratchpads = foldr1 keyComb
    [ wmBinds
    , spawnBinds
    , menuKeys $ guiMenu hostConfig
    , workspaceBinds
    , screenBinds
    , scratchpadsBinds scratchpads
    ]
    where
        keyComb f g conf = M.union (f conf) (g conf)

menuKeys :: String -> KeyConfig
menuKeys "rofi" (XConfig {modMask = modm}) = M.fromList $
    [ ((0, xF86XK_Launch1),          safeSpawn "rofi" ["-show", "run"])
    , ((modm, xK_r),                 safeSpawn "rofi" ["-show", "run"])
    , ((modm .|. controlMask, xK_p), safeSpawn "rofi-pass" [])
    , ((modm .|. controlMask, xK_s), safeSpawn "rofi" ["-show", "ssh"])
    ]
menuKeys "dmenu" (XConfig {modMask = modm}) = M.fromList $
    [ ((0, xF86XK_Launch1),          safeSpawn "dmenu_run" [])
    , ((modm, xK_r),                 safeSpawn "dmenu_run" [])
    , ((modm .|. controlMask, xK_p), safeSpawn "passmenu" ["--type"])
    , ((modm .|. controlMask, xK_o), safeSpawn "clipmenu" [])
    ]
menuKeys _ _ = M.fromList []

scratchpadsBinds :: NamedScratchpads -> KeyConfig
scratchpadsBinds scratchpads (XConfig {modMask = modm}) = M.fromList . map mkBind $
    [ ((mod1Mask, xK_Return), "scratchpad")
    , ((mod1Mask, xK_m),      "mixer")
    , ((mod1Mask, xK_f),      "files")
    , ((mod1Mask, xK_w),      "whatsapp")
    , ((mod1Mask, xK_c),      "calendar")
    , ((mod1Mask, xK_p),      "player")
    , ((mod1Mask, xK_e),      "telegram")
    , ((mod1Mask, xK_r),      "signal")
    , ((mod1Mask, xK_u),      "thunderbird")
    , ((mod1Mask, xK_g),      "hangouts")
    , ((mod1Mask, xK_t),      "top")
    , ((modm .|. controlMask, xK_b), "bookmarks")
    ]
    where mkBind (key,app) = (key, namedScratchpadAction scratchpads app)

spawnBinds :: KeyConfig
spawnBinds conf = M.fromList . map mkSpawn $ bindList
    where
        bindList = singles ++ mpc ++ xbacklight ++ amixer
        singles =
            [ ((modm, xK_Return),             terminal conf,     [])
            , ((0, xF86XK_RotateWindows),     "thinkpad-rotate", [])
            , ((modm, xK_Escape),             "slock",           [])
            ]
        mpc = withCmd "mpc"
            [ ((0, xK_KP_End),        ["prev"])
            , ((0, xK_KP_Down),       ["toggle"])
            , ((0, xK_KP_Next),       ["next"])
            , ((0, xF86XK_AudioPlay), ["toggle"])
            , ((0, xF86XK_AudioStop), ["stop"])
            , ((0, xF86XK_AudioPrev), ["prev"])
            , ((0, xF86XK_AudioNext), ["next"])
            ]
        xbacklight = withCmd "xbacklight"
            [ ((0, xF86XK_MonBrightnessUp),   ["-inc", "10"])
            , ((0, xF86XK_MonBrightnessDown), ["-dec", "10"])
            ]
        amixer = withCmd "amixer"
            [ ((0, xK_KP_Subtract),          ["set", "Master", "5%-"])
            , ((0, xK_KP_Add),               ["set", "Master", "5%+"])
            , ((0, xK_KP_Enter),             ["set", "Master", "100%"])
            , ((0, xK_KP_Insert),            ["set", "Master", "toggle"])
            , ((0, xF86XK_AudioLowerVolume), ["set", "Master", "5%-"])
            , ((0, xF86XK_AudioRaiseVolume), ["set", "Master", "5%+"])
            , ((0, xF86XK_AudioMute),        ["set", "Master", "toggle"])
            ]
        mkSpawn (comb,cmd,args) = (comb, safeSpawn cmd args)
        withCmd cmd = map (\(comb,args) -> (comb,cmd,args))
        modm = modMask conf

wmBinds :: KeyConfig
wmBinds conf@ XConfig {modMask = modm} = M.fromList $
    [ ((0, xK_Insert), pasteSelection)

    , ((modm .|. shiftMask, xK_o), restart "obtoxmd" True)
    , ((modm .|. shiftMask, xK_r), restart "xmonad" True)
    , ((modm .|. shiftMask, xK_Escape), io (exitWith ExitSuccess))

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

    , ((modm,               xK_Right ), nextWS')
    , ((modm,               xK_Left  ), prevWS')
    , ((modm .|. shiftMask, xK_Right ), shiftToNext')
    , ((modm .|. shiftMask, xK_Left  ), shiftToPrev')

    , ((modm, xK_0), toggleWS' ["NSP"])
    ]

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

workspaceBinds :: KeyConfig
workspaceBinds conf@ XConfig {modMask = modm} = M.fromList $
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(greedyView, 0), (shift, shiftMask)]]

screenBinds :: KeyConfig
screenBinds XConfig {modMask = modm} = M.fromList $
    [((m .|. modm, k), screenWorkspace i >>= flip whenJust (windows . f))
       | (i, k) <- zip [0,1] [xK_w, xK_e]
       , (f, m) <- [(view, 0), (shift, shiftMask)]]
