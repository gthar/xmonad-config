module MyPP (mkPP) where

import XMonad.Util.Run (hPutStrLn)

import GHC.IO.Handle.Types (Handle)

import Data.List (intercalate, isPrefixOf)
import XMonad.Util.NamedWindows (getName)

import XMonad.Core
    ( Layout
    , ScreenDetail
    , ScreenId (S)
    , withWindowSet
    , WorkspaceId
    , WindowSet
    , description
    )

import XMonad.Config (def)

import Graphics.X11.Types (Window)

import XMonad.StackSet
    ( Workspace (..)
    , screen
    , workspace
    , current
    )
import qualified XMonad.StackSet as S

import Text.Printf (printf)

import XMonad.Util.Loggers (Logger)

import XMonad.Hooks.DynamicLog
    ( PP
    , ppCurrent
    , ppExtras
    , ppHidden
    , ppHiddenNoWindows
    , ppLayout
    , ppOrder
    , ppOutput
    , ppSep
    , ppTitle
    , ppUrgent
    , ppVisible
    , shorten
    , wrap
    , xmobarAction
    , xmobarColor
    )

import Theme
    ( inactiveColor
    , defFg
    , selFg
    , selectionColor
    , urgentColor
    )


mkPP :: Bool -> Bool -> [String] -> Handle -> Int -> PP
mkPP selCompleteTaskbar selShowLayout ws bar nscreen = common
    { ppOutput = hPutStrLn bar
    , ppExtras = extras
    }
    where
        common = def
            { ppCurrent         = const ""
            , ppVisible         = const ""
            , ppHidden          = const ""
            , ppHiddenNoWindows = const ""
            , ppUrgent          = xmobarColor urgentColor ""
            , ppOrder           = order
            , ppTitle           = title selCompleteTaskbar
            , ppSep             = xmobarColor inactiveColor "" "|"
            , ppLayout          = const ""
            }

        extras = wsLog:xs
            where
                wsLog = workspaceLog ws nscreen
                xs = optsFilter [selShowLayout, selCompleteTaskbar] opts
                opts = [layoutLog nscreen, taskbar nscreen]
                optsFilter sel = map snd . filter fst . zip sel

        order (_:_:t:xs) = xs ++ [t]
        order _ = ["error setting xmobar order"]

        title True  = const ""
        title False = wrap " " "" . setColor . wrap " " " " . shorten 80
            where setColor = xmobarColor selFg selectionColor


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

workspaceLog :: [String] -> Int -> Logger
workspaceLog ws i = withWindowSet $ return . Just . wsFun i
    where
        wsFun j x = fun ws
            where
                fun = (++" ") . intercalate sep . shownFilter . fmtWs

                myWs = tag $ S.workspace $ getScreen j x
                wsStates = map (getWsState myWs (S.workspaces x)) ws

                colorer = zipWith (addColor focusedScreen) wsStates
                focusedScreen = isScreenFocused x j
                rmScreenTag (_:'_':xs) = xs
                rmScreenTag _ = "?"
                fmtWs = colorer . map rmScreenTag

                screenFilter = map (isPrefixOf ((show i) ++ "_")) ws
                inactiveFilter = map (/=2) wsStates
                shownFilter = map snd . filter fst . zip sel
                    where
                        sel = zipWith (&&) screenFilter inactiveFilter

        addColor True  0 = xmobarColor selFg selectionColor
        addColor False 0 = xmobarColor selectionColor ""
        addColor _     1 = xmobarColor defFg ""
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
