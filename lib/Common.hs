{-# LANGUAGE OverloadedStrings #-}

module Common
    ( HostConfig (HostConfig)
    , completeTaskbar
    , showLayout
    ) where

import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Y

data HostConfig = HostConfig
    { completeTaskbar :: Bool
    , showLayout      :: Bool
    }

instance FromJSON HostConfig where
    parseJSON (Y.Object v) = HostConfig <$>
        v .: "completeTaskbar" <*>
        v .: "showLayout"
    parseJSON _ = fail "Error while parsing config"
