module Handler.Reviews (getReviewsR) where

import Import
import Helpers.Model
import Helpers.User
import Helpers.Grade

getReviewsR :: Handler RepHtml
getReviewsR = do
    records <- runDB $ do
        reviews   <- selectList [] [Desc ReviewCreatedDate] 
        landlords <- selectList [] []
        users     <- selectList [] []

        return $ joinTables3 reviewLandlord reviewReviewer reviews landlords users

    defaultLayout $ do
        setTitle "All reviews"
        addWidget $(widgetFile "review/index")

reviewWidget :: ReviewId -> Review -> Landlord -> User -> Widget
reviewWidget rid r l u = $(widgetFile "review/_review_row")
