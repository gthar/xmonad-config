module Common
    ( HostConfig (HostConfig)
    , guiMenu
    , completeTaskbar
    , showLayout
    ) where

data HostConfig = HostConfig
    { guiMenu         :: String
    , completeTaskbar :: Bool
    , showLayout      :: Bool
    }
