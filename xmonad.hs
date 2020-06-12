{-# LANGUAGE OverloadedStrings #-}

import HostConfig (readHostConfig)
import MyConfig (mkMain)

main :: IO ()
main = readHostConfig >>= mkMain
