{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (postSearchR) where

import Yesod
import BadLandlords

postSearchR :: Handler RepHtml
postSearchR = do
    landlord <- runFormPost' $ stringInput "landlord"
    defaultLayout $ do
        setTitle "bad boston landlords | Search complaints"
        addHamlet [$hamlet|
            <h2>#{landlord}

            <p>
                Todo:
            |]
