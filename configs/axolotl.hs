import MyConfig (mkMain)
import DefaultConfig
    ( mkPP
    , wsNamer
    )

main :: IO ()
main = mkMain (mkPP wsNamer False) "Inconsolata for Powerline:size=12"
