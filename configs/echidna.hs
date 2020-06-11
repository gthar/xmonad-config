import MyConfig (mkMain)

import Common
    ( HostConfig (HostConfig)
    , completeTaskbar
    , showLayout
    )


hostConfig :: HostConfig
hostConfig = HostConfig
    { completeTaskbar = True
    , showLayout      = True
    }

main :: IO ()
main = mkMain hostConfig
