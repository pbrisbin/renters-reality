{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.New (getNewR, postNewR) where

import Foundation
import Helpers.Forms

getNewR :: Handler RepHtml
getNewR = do
    (uid, _) <- requireAuth
    ml       <- lookupGetParam "landlord"
    defaultLayout $ do
        setTitle "New review"
        addWidget $(widgetFile "new")

postNewR :: Handler RepHtml
postNewR = getNewR
