module Helpers.Request (requestIp) where

import Prelude
import Yesod
import Network.Wai (remoteHost)
import Data.Text   (Text)
import qualified Data.Text as T

requestIp :: GHandler sub master Text
requestIp = return . T.pack . show . remoteHost =<< waiRequest
