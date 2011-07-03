{-# LANGUAGE OverloadedStrings #-}
module Handlers
    ( getFaviconR
    , getRobotsR
    , module X
    ) where
    
import Yesod
import Renters

import Handlers.Root      as X
import Handlers.Legal     as X
import Handlers.Search    as X
import Handlers.New       as X
import Handlers.Profile   as X
import Handlers.Reviews   as X
import Handlers.Landlords as X
import Handlers.Feed      as X

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)

