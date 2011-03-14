{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Legal (getLegalR) where

import Yesod
import BadLandlords

getLegalR :: Handler RepHtml
getLegalR = defaultLayout $ do
    setTitle "bad boston landlords | Legal"
    addHamlet [$hamlet|
        <h2>Legal information

        <p>
            At this time, we simply cannot guarantee the accuracy of any 
            information on this site.
        |]
