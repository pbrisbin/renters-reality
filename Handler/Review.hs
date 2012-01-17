module Handler.Review
    ( getReviewR
    , postReviewR
    , getEditR
    , postEditR
    , getNewR
    , postNewR
    ) where

import Import
import Helpers.Request
import Helpers.Review
import Yesod.Markdown
import Data.Time.Format.Human

getReviewR :: ReviewId -> Handler RepHtml
getReviewR rid = do
    docs <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d -> do
            ownReview  <- maybeReviewer d
            reviewTime <- liftIO . humanReadableTime . reviewCreatedDate $ review d
            defaultLayout $ do
                setTitle "View review"
                addWidget $(widgetFile "review/show")

        Nothing -> notFound

getEditR :: ReviewId -> Handler RepHtml
getEditR rid = do
    docs <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d@(Document _ r l _) -> do
            requireReviewer d

            ip <- requestIp

            ((res, form), enctype) <- runFormPost $ reviewForm (Just r) (Just $ landlordName l) ip
            doAndRedirect res (updateReview rid)

            defaultLayout $ do
                setTitle "Edit review"
                addWidget $(widgetFile "review/edit")

        _ -> notFound

getNewR :: Handler RepHtml
getNewR = do
    (uid, _) <- requireAuth
    ml       <- lookupGetParam "landlord"
    ip       <- requestIp

    ((res, form), enctype) <- runFormPost $ reviewForm Nothing ml ip
    doAndRedirect res (insertReview uid)

    defaultLayout $ do
        setTitle "New review"
        addWidget $(widgetFile "review/new")

postReviewR :: ReviewId -> Handler RepHtml
postReviewR = getReviewR

postNewR :: Handler RepHtml
postNewR = getNewR

postEditR :: ReviewId -> Handler RepHtml
postEditR = getEditR
