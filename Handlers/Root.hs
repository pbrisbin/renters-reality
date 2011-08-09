{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Root (getRootR) where

import Renters
import Helpers.Widgets
import Yesod.Helpers.Auth

getRootR :: Handler RepHtml
getRootR = do
    muid <- maybeAuthId
    defaultLayout $ do
        setTitle "Home"
        addWidget $(widgetFile "homepage")

        addAutoCompletion "#search-input"   CompSearchesR
        addAutoCompletion "#landlord-input" CompLandlordsR
        addHelpBox $ addWidget $(widgetFile "search-help")
