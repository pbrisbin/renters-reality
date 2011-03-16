{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (postNewR) where

import Yesod
import BadLandlords
import Model
import Forms

postNewR :: Handler RepHtml
postNewR = do
    landlord <- landlordFromForm
    defaultLayout $ do
        setTitle "bad boston landlords | New complaint"
        [$hamlet|
            <h2>New complaint about #{landlordName landlord}

            <div .tabdiv>
                <div .tabcontent>
                    ^{complaintForm $ landlordName landlord}
            |]
