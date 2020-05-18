import MyConfig (mkMain)
import DefaultConfig (defaultPP)

main :: IO ()
main = mkMain defaultPP "Inconsolata for Powerline:size=12"
