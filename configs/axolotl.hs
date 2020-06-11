import MyConfig (mkMain)

import Common
    ( HostConfig (HostConfig)
    , completeTaskbar
    , showLayout
    )


hostConfig :: HostConfig
hostConfig = HostConfig
    { completeTaskbar = False
    , showLayout      = True
    }

main :: IO ()
main = mkMain hostConfig
