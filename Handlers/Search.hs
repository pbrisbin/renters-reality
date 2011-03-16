{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (postSearchR) where

import Yesod
import BadLandlords
import Forms
import Model

import Data.List  (intercalate)
import Data.Maybe (fromMaybe)

postSearchR :: SearchType -> Handler RepHtml
postSearchR _ = notFound
