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
            <h2>New complaint: #{landlordName landlord}

            <div .tabdiv>
                <div .tabcontent>
                    <form method="post" action=@{CreateR}>
                        <input type=hidden name="landlord" value=#{landlordName landlord}>

                        <h3>Who you are:

                        <table>
                            ^{userInfoFields}

                        <h3>Where you live:

                        <table>
                            ^{addressFormFields}

                        <h3>Your complaint:

                        <textarea rows="20" cols="60" name="content" required>
            |]

