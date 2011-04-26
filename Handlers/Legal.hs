{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Legal (getLegalR) where

import Yesod
import Renters

import qualified Settings

getLegalR :: Handler RepHtml
getLegalR = defaultLayout $ do
    Settings.setTitle "Legal information"
    addHamlet [hamlet|
        <h1>Legal information
        <div .tabdiv>
            <h3>Content
            <p>
                The content on this site is entered by its users. It 
                represents their opinions and beliefs.

            <p>
                We make no attempt to validate these claims.


            <h3>Fair use
            <p>
                Content that fits the following categories will be 
                removed immediately.

            <ul>
                <li>Offensive language
                <li>Defamation of character
                <li>Name calling
                <li>Bickering

            <p>
                That said, we reserve the right to remove any and 
                all content, at any time, for no reason and at our 
                sole discretion.
        |]
