{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Create (postCreateR) where

import Yesod
import Renters
import Forms
import Model

postCreateR :: ReviewType -> Handler RepHtml
postCreateR rtype = do
    review <- reviewFromForm rtype
    _      <- findOrCreate review
    redirect RedirectTemporary $ ReviewsR (reviewReference review)
