{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Reviews 
    ( getReviewsR
    , postReviewsR -- comments
    ) where

import Renters
import Model
import Yesod
import Yesod.Helpers.Auth
import Helpers.Widgets
import Database.Persist.Base
import Yesod.Comments
import qualified Data.Text as T
import qualified Settings

getReviewsR :: Key Review -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d -> defaultLayout $ do
            Settings.setTitle "View review"
            [hamlet|
                <div .tabdiv>
                    <div .view-review>
                        ^{landlordGrade d}
                        ^{reviewContentBlock d False}
                        ^{reviewedBy "reviewed-by" d $ editLink d}

                    <h3>Discussion
                    <div .discussion>
                        ^{addCommentsAuth $ rText rid}
                |]

        Nothing -> notFound

    where
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

rText :: ReviewId -> T.Text
rText = go . unReviewId

    where
        go (PersistText  t) = t
        go (PersistInt64 i) = T.pack $ show i
        go _                = ""

postReviewsR :: ReviewId -> Handler RepHtml
postReviewsR = getReviewsR
