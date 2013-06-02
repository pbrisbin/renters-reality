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
import Yesod.Auth
import Yesod.Markdown
import Yesod.Comments (addComments)
import Data.Time.Format.Human

getReviewR :: ReviewId -> Handler RepHtml
getReviewR rid = do
    (review,landlord,user) <- getReviewRecords rid

    ownReview  <- maybeReviewer review
    reviewTime <- liftIO . humanReadableTime $ reviewCreatedDate review

    defaultLayout $ do
        setTitle "View review"
        $(widgetFile "review/show")

getEditR :: ReviewId -> Handler RepHtml
getEditR rid = do
    (review,landlord,user) <- getReviewRecords rid

    requireReviewer rid review

    ip <- requestIp

    ((res, form), enctype) <- runFormPost $ reviewForm (Just review) (Just $ landlordName landlord) ip
    doAndRedirect res (updateReview rid)

    defaultLayout $ do
        setTitle "Edit review"
        $(widgetFile "review/form")

getNewR :: Handler RepHtml
getNewR = do
    (Entity uid _) <- requireAuth
    ml             <- lookupGetParam "landlord"
    ip             <- requestIp

    ((res, form), enctype) <- runFormPost $ reviewForm Nothing ml ip
    doAndRedirect res (insertReview uid)

    defaultLayout $ do
        setTitle "New review"
        $(widgetFile "review/form")

postReviewR :: ReviewId -> Handler RepHtml
postReviewR = getReviewR

postNewR :: Handler RepHtml
postNewR = getNewR

postEditR :: ReviewId -> Handler RepHtml
postEditR = getEditR

getReviewRecords :: ReviewId -> Handler (Review, Landlord, User)
getReviewRecords rid = runDB $ do
    r <- get404 rid
    l <- get404 $ reviewLandlord r
    u <- get404 $ reviewReviewer r

    return (r,l,u)
