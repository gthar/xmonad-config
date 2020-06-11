module Common
    ( HostConfig (HostConfig)
    , completeTaskbar
    , showLayout
    ) where

data HostConfig = HostConfig
    { completeTaskbar :: Bool
    , showLayout      :: Bool
    }
