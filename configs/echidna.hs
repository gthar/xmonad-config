import MyConfig (mkMain)
import DefaultConfig (defaultPP)

main :: IO ()
main = mkMain defaultPP "st" "Inconsolata for Powerline:size=12"
