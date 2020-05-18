import MyConfig (mkMain)
import DefaultConfig (defaultPP)

main :: IO ()
main = mkMain defaultPP "alacritty" "Inconsolata for Powerline:size=14"
