{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (postSearchR) where

import Yesod
import BadLandlords

postSearchR :: String -> Handler RepHtml
postSearchR "landlord" = do
    landlord <- runFormPost' $ stringInput "landlord"
    defaultLayout $ do
        setTitle "bad boston landlords | Search complaints"
        addHamlet [$hamlet|
            <h2>#{landlord}

            <p>
                Todo:
            |]

postSearchR "property" = undefined

postSearchR _ = notFound
