{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Reviews 
    ( getReviewsR
    , postReviewsR
    ) where

import Foundation
import Helpers.Widgets
import Yesod.Comments
import Database.Persist.Base
import Data.Text (Text)
import qualified Data.Text as T

getReviewsR :: ReviewId -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d -> defaultLayout $ do
            setTitle "View review"
            addWidget $(widgetFile "reviews")

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
