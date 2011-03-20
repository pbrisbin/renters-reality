{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (postNewR) where

import Yesod
import BadLandlords
import Model
import Forms

import qualified Settings

postNewR :: ReviewType -> Handler RepHtml
postNewR rtype = do
    landlord <- landlordFromForm
    defaultLayout $ do
        Settings.setTitle $ "New: " ++ landlordName landlord
        [$hamlet|
            <h2>New review for #{landlordName landlord}

            <div .tabdiv>
                <div .tabcontent>
                    ^{reviewForm landlord rtype}
            |]
