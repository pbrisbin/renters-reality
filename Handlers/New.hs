{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (postNewR) where

import Yesod
import Renters
import Model
import Forms

import qualified Settings

postNewR :: ReviewType -> Handler RepHtml
postNewR rtype = do
    req <- getRequest
    case getParam req "landlord" of
        Nothing -> undefined
        Just landlord -> defaultLayout $ do
            Settings.setTitle $ "New review: " ++ landlord
            [hamlet|
                <h2>New review for #{landlord}

                <div .tabdiv>
                    <div .tabcontent>
                        ^{reviewForm (Landlord landlord) rtype}
                |]
