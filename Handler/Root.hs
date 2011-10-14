{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Root (getRootR) where

import Foundation
import Helpers.Widgets

getRootR :: Handler RepHtml
getRootR = do
    muid <- maybeAuthId
    defaultLayout $ do
        setTitle "Home"
        addWidget $(widgetFile "homepage")

        addAutoCompletion "#search-input"   CompSearchesR
        addAutoCompletion "#landlord-input" CompLandlordsR
        addHelpBox $ addWidget $(widgetFile "search-help")
