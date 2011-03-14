{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (postNewR) where

import Yesod
import BadLandlords

postNewR :: Handler RepHtml
postNewR = do
    landlord <- runFormPost' $ stringInput "landlord"
    defaultLayout $ do
        setTitle "bad boston landlords | New complaint"
        addHamlet [$hamlet|
            <h2>#{landlord}

            <p>
                Todo:
            |]
