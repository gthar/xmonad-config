import MyConfig
    ( mkMain
    , dmenuFont
    , term
    , completeTaskbar
    , showLayout
    , HostConfig (HostConfig)
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
