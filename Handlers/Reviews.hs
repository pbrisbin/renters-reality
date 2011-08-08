{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Reviews 
    ( getReviewsR
    , postReviewsR
    ) where

import Renters
import Yesod.Helpers.Auth
import Helpers.Widgets
import Database.Persist.Base
import Yesod.Comments
import Data.Text (Text)
import qualified Data.Text as T

getReviewsR :: Key Review -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d -> defaultLayout $ do
            setTitle "View review"
            addWidget $(widgetFile "reviews")

        Nothing -> notFound

postReviewsR :: ReviewId -> Handler RepHtml
postReviewsR = getReviewsR

editLink :: Document -> Widget ()
editLink (Document rid r _ _) = do
    muid <- lift $ maybeAuth
    case muid of
        Just (uid,_) ->
            if uid == reviewReviewer r
                then [hamlet|
                    <span .edit-link>
                        <a href="@{EditR rid}">EDIT
                    |]
                else return ()

        _ -> return ()

rText :: ReviewId -> Text
rText = go . unReviewId

    where
        go (PersistText  t) = t
        go (PersistInt64 i) = T.pack $ show i
        go _                = ""
