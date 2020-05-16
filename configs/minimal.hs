import MyConfig (mkMain)
import DefaultConfig
    ( mkPP
    , wsNamer
    )

main :: IO ()
main = mkMain $  mkPP wsNamer False
