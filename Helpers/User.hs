module Helpers.User (showName) where

import Prelude
import Model
import Data.Shorten
import Data.Text (Text)

showName :: User -> Text
showName (User (Just fn) _         _ _ _) = shorten 40 fn
showName (User _         (Just un) _ _ _) = shorten 40 un
showName _                                = "anonymous"
