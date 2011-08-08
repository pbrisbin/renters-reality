{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (getNewR, postNewR) where

import Renters
import Helpers.Forms
import Yesod.Helpers.Auth

getNewR :: Handler RepHtml
getNewR = do
    (uid, _) <- requireAuth
    ml       <- lookupGetParam "landlord"
    defaultLayout $ do
        setTitle "New review"
        addWidget $(widgetFile "new")

postNewR :: Handler RepHtml
postNewR = getNewR
