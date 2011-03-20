{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Legal (getLegalR) where

import Yesod
import BadLandlords

import qualified Settings

getLegalR :: Handler RepHtml
getLegalR = defaultLayout $ do
    Settings.setTitle "Legal information"
    addHamlet [$hamlet|
        <h2>Legal information
        <div .tabdiv>
            <div .tabcontent>
                <h3>Content
                <p>
                    At this time, we simply cannot guarantee the accuracy of any 
                    information on this site. We are in a dedicated 
                    testing phase where users are encouraged to enter 
                    fake data to test the basic functionality of the 
                    site.

                <p>
                    Any names or descriptions that resemble real-life 
                    personas are purly coincidental.

                <h3>Fair use
                <p>
                    Below are a few things we will not tolerate on this 
                    site now or when we go live. Any user-provided 
                    content that fits in the following categories will 
                    be removed immediately.

                <ul>
                    <li>Foul language
                    <li>Defamation of character
                    <li>Name calling
                    <li>Bickering

                <p>
                    That said, we reserve the right to remove any and 
                    all content, at any time, for no reason and at our 
                    sole descretion.
        |]
