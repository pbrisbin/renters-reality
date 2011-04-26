-- Makes testing easier
module Handlers
    ( getFaviconR
    , getRobotsR
    , module Handlers.Root
    , module Handlers.Legal
    , module Handlers.Search
    , module Handlers.New
    , module Handlers.Profile
    , module Handlers.Reviews
    ) where
    
import Handlers.Root
import Handlers.Legal
import Handlers.Search
import Handlers.New
import Handlers.Profile
import Handlers.Reviews

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)

