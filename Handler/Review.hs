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
import Helpers.User
import Helpers.Grade
import Yesod.Markdown
import Yesod.Comments (addCommentsAuth)
import Data.Time.Format.Human

getReviewR :: ReviewId -> Handler RepHtml
getReviewR rid = do
    (review,landlord,user)<- runDB $ do
        r <- get404 rid
        l <- get404 $ reviewLandlord r
        u <- get404 $ reviewReviewer r

        return (r,l,u)

    ownReview  <- maybeReviewer review
    reviewTime <- liftIO . humanReadableTime $ reviewCreatedDate review

    defaultLayout $ do
        setTitle "View review"
        addWidget $(widgetFile "review/show")

getEditR :: ReviewId -> Handler RepHtml
getEditR rid = do
    (review,landlord,user) <- runDB $ do
        r <- get404 rid
        l <- get404 $ reviewLandlord r
        u <- get404 $ reviewReviewer r

        return (r,l,u)

    requireReviewer rid review

    ip <- requestIp

    ((res, form), enctype) <- runFormPost $ reviewForm (Just review) (Just $ landlordName landlord) ip
    doAndRedirect res (updateReview rid)

    defaultLayout $ do
        setTitle "Edit review"
        addWidget $(widgetFile "review/edit")


getNewR :: Handler RepHtml
getNewR = do
    (Entity uid _) <- requireAuth
    ml             <- lookupGetParam "landlord"
    ip             <- requestIp

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
