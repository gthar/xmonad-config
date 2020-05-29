import MyConfig (mkMain)

import Common
    ( HostConfig (HostConfig)
    , dmenuFont
    , term
    , completeTaskbar
    , showLayout
    )


hostConfig :: HostConfig
hostConfig = HostConfig
    { dmenuFont       = "Inconsolata:size=12"
    , term            = "st"
    , completeTaskbar = False
    , showLayout      = False
    }

main :: IO ()
main = mkMain hostConfig
