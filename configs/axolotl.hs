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
    { dmenuFont       = "Inconsolata for Powerline:size=12"
    , term            = "alacritty"
    , completeTaskbar = False
    , showLayout      = True
    }

main :: IO ()
main = mkMain hostConfig
