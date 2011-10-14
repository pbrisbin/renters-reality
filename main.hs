import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withRenters)

main :: IO ()
main = defaultMain fromArgs withRenters