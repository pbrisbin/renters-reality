{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Create (postCreateR) where

import Yesod
import BadLandlords
import Forms
import Model

postCreateR :: Handler RepHtml
postCreateR = do
    complaint <- complaintFromForm
    _         <- findOrCreate complaint
    redirect RedirectTemporary $ ComplaintsR (complaintReference complaint)
