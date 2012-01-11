module Handler.Reviews 
    ( getReviewsR
    , postReviewsR
    , getEditR
    , postEditR
    , getNewR
    , postNewR
    ) where

import Import
import Helpers.Forms

import Yesod.Goodies
import Control.Monad (unless)
import Database.Persist.Store (PersistValue(PersistText, PersistInt64))
import qualified Data.Text as T

getReviewsR :: ReviewId -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d -> do
            reviewTime <- liftIO . humanReadableTime . reviewCreatedDate $ review d
            ownReview  <- isReviewer d
            defaultLayout $ do
                setTitle "View review"
                addWidget $(widgetFile "review/show")

        Nothing -> notFound

    where
        isReviewer :: Document -> Handler Bool
        isReviewer (Document _ r _ _) = do
            muid <- maybeAuth
            return $ case muid of
                Just (uid,_) -> uid == reviewReviewer r
                _            -> False

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
                redirect $ tm (ReviewsR rid)

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
