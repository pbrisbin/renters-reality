{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Complaints 
    ( getComplaintsR
    , postComplaintsR -- comments
    ) where

import Yesod
import BadLandlords

getComplaintsR :: Int -> Handler RepHtml
getComplaintsR ref = defaultLayout $ do
    setTitle "bad boston landlords | View complaint"
    [$hamlet|
        <h2>Complaint ##{show ref}
        <div .tabdiv>
            <div .tabcontent>
                <p>Todo:
        |]

postComplaintsR :: Int -> Handler RepHtml
postComplaintsR = getComplaintsR
