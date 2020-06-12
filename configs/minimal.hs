import MyConfig (mkMain)

import Common
    ( HostConfig (HostConfig)
    , guiMenu
    , completeTaskbar
    , showLayout
    )


hostConfig :: HostConfig
hostConfig = HostConfig
    { guiMenu         = "dmenu"
    , completeTaskbar = False
    , showLayout      = False
    }

main :: IO ()
main = mkMain hostConfig
