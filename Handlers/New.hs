{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (getNewR, postNewR) where

import Renters
import Helpers.Forms
import Helpers.Widgets
import Yesod.Helpers.Auth
import Network.Wai (remoteHost)
import qualified Data.Text as T

getNewR :: Handler RepHtml
getNewR = do
    (uid, _) <- requireAuth

    ml <- lookupGetParam "landlord"
    defaultLayout $ do
        setTitle "New review"
        addWidget $(widgetFile "new")
        addAutoCompletion "input#landlord" CompLandlordsR
        addHelpBox helpBoxContents

postNewR :: Handler RepHtml
postNewR = getNewR

runReviewForm :: UserId -> Maybe T.Text -> Widget ()
runReviewForm uid ml = do
    ip <- lift $ return . T.pack . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- lift . runFormMonadPost $ reviewForm Nothing ml ip
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess rf -> lift $ do
            tm  <- getRouteToMaster
            rid <- insertFromForm uid rf
            redirect RedirectTemporary $ tm (ReviewsR rid)

    [hamlet|<form enctype="#{enctype}" method="post">^{form}|]
