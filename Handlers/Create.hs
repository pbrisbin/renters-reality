{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Create (postCreateR) where

import Yesod
import BadLandlords
import Forms
import Model

postCreateR = do
    landlord <- landlordFromForm
    addr     <- addressFromForm
    user     <- userInfoFromForm
    
    landlordId <- findOrCreateLandlord landlord
    propertyId <- findOrCreateProperty addr

    undefined
