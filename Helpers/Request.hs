module Helpers.Request (requestIp) where

import Prelude
import Yesod
import Network.Wai (remoteHost)
import Data.Text   (Text)
import qualified Data.Text as T

requestIp :: HandlerT master IO Text
requestIp = return . T.pack . show . remoteHost =<< waiRequest
