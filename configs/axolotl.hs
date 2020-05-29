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
    { dmenuFont       = "Inconsolata for Powerline:size=12"
    , term            = "st"
    , completeTaskbar = False
    , showLayout      = True
    }

main :: IO ()
main = mkMain hostConfig
