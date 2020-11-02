{-# LANGUAGE OverloadedStrings #-}

module HostConfig
    ( HostConfig (HostConfig)
    , readHostConfig
    , fontSize
    , completeTaskbar
    , showLayout
    ) where

import Data.Yaml (FromJSON(..), (.:))
import System.Environment (getEnv)
import Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.Yaml as Y

data HostConfig = HostConfig
    { fontSize        :: Int
    , completeTaskbar :: Bool
    , showLayout      :: Bool
    }

instance FromJSON HostConfig where
    parseJSON (Y.Object v) = HostConfig <$>
        v .: "fontSize" <*>
        v .: "completeTaskbar" <*>
        v .: "showLayout"
    parseJSON _ = fail "Error while parsing config"

getConfFile :: IO String
getConfFile = printf "%s/.my-xmonad.yml" <$> getEnv "HOME"

readHostConfig :: IO HostConfig
readHostConfig = getConfFile >>= B.readFile >>= Y.decodeThrow
