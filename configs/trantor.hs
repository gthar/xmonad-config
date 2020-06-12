import MyConfig (mkMain)

import Common
    ( HostConfig (HostConfig)
    , guiMenu
    , completeTaskbar
    , showLayout
    )


hostConfig :: HostConfig
hostConfig = HostConfig
    { guiMenu         = "rofi"
    , completeTaskbar = True
    , showLayout      = True
    }

main :: IO ()
main = mkMain hostConfig
