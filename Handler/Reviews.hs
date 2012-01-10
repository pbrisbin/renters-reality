{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Reviews 
    ( getReviewsR
    , postReviewsR
    , getEditR
    , postEditR
    , getNewR
    , postNewR
    ) where

import Foundation
import Helpers.Forms
import Helpers.Widgets
import Yesod.Comments
import Control.Monad (unless)
import Database.Persist.Base
import Data.Text (Text)
import qualified Data.Text as T

getReviewsR :: ReviewId -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d -> defaultLayout $ do
            setTitle "View review"
            addWidget $(widgetFile "review/show")

        Nothing -> notFound

    where
        editLink :: Document -> Widget
        editLink (Document _ r _ _) = do
            muid <- lift $ maybeAuth
            case muid of
                Just (uid,_) ->
                    if uid == reviewReviewer r
                        then [whamlet|
                            <span .edit-link>
                                <a href="@{EditR rid}">EDIT
                            |]
                        else return ()

                _ -> return ()

        rText :: ReviewId -> Text
        rText = go . unKey

            where
                go (PersistText  t) = t
                go (PersistInt64 i) = T.pack $ show i
                go _                = ""



postReviewsR :: ReviewId -> Handler RepHtml
postReviewsR = getReviewsR

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
                addWidget $(widgetFile "review/edit")

        _ -> notFound

postEditR :: ReviewId -> Handler RepHtml
postEditR = getEditR

getNewR :: Handler RepHtml
getNewR = do
    (uid, _) <- requireAuth
    ml       <- lookupGetParam "landlord"
    defaultLayout $ do
        setTitle "New review"
        addWidget $(widgetFile "review/new")

postNewR :: Handler RepHtml
postNewR = getNewR
