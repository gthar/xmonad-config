module Common
    ( HostConfig (HostConfig)
    , dmenuFont
    , term
    , completeTaskbar
    , showLayout
    ) where

data HostConfig = HostConfig
    { dmenuFont       :: String
    , term            :: String
    , completeTaskbar :: Bool
    , showLayout      :: Bool
    }
