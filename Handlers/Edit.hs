{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Edit
    ( getEditR
    , postEditR
    ) where

import Renters
import Helpers.Forms
import Helpers.Widgets
import Yesod.Helpers.Auth
import Control.Monad (unless)
import Network.Wai (remoteHost)
import qualified Data.Text as T

getEditR :: ReviewId -> Handler RepHtml
getEditR rid = do
    (uid, _) <- requireAuth
    docs     <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d -> do
            -- not your review, redirect to the view page
            unless (uid == (reviewReviewer $ review d)) $ do
                tm <- getRouteToMaster
                redirect RedirectTemporary $ tm (ReviewsR rid)

            -- allow edit
            defaultLayout $ do
                setTitle "Edit review"
                addWidget $(widgetFile "edit")

                -- java script tricks
                addAutoCompletion "input#landlord" CompLandlordsR
                addHelpBox helpBoxContents

        _ -> notFound

postEditR :: ReviewId -> Handler RepHtml
postEditR = getEditR

runReviewForm :: Document -> UserId -> Widget ()
runReviewForm (Document rid r l _) uid = do
    ip <- lift $ return . T.pack . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- lift . runFormMonadPost $ reviewForm (Just r) (Just $ landlordName l) ip
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess rf -> lift $ do
            tm  <- getRouteToMaster
            _   <- updateFromForm rid uid rf
            redirect RedirectTemporary $ tm (ReviewsR rid)

    [hamlet|<form enctype="#{enctype}" method="post">^{form}|]
