{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Edit
    ( getEditR
    , postEditR
    ) where

import Renters
import Helpers.Forms
import Yesod.Helpers.Auth
import Control.Monad (unless)

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

            defaultLayout $ do
                setTitle "Edit review"
                addWidget $(widgetFile "edit")

        _ -> notFound

postEditR :: ReviewId -> Handler RepHtml
postEditR = getEditR
