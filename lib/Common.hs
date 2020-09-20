{-# LANGUAGE OverloadedStrings #-}

module Common
    ( HostConfig (HostConfig)
    , guiMenu
    , completeTaskbar
    , showLayout
    ) where

import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y

data HostConfig = HostConfig
    { guiMenu         :: String
    , completeTaskbar :: Bool
    , showLayout      :: Bool
    }

instance FromJSON HostConfig where
    parseJSON (Y.Object v) = HostConfig <$>
        v .: "guiMenu" <*>
        v .: "completeTaskbar" <*>
        v .: "showLayout"
    parseJSON _ = fail "Error while parsing config"
