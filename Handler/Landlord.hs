module Handler.Landlord (getLandlordR) where

import Import
import Helpers.Model
import Helpers.User
import Helpers.Grade
import Data.Time.Format.Human
import qualified Data.Text as T

getLandlordR :: LandlordId -> Handler RepHtml
getLandlordR lid = do
    (landlord,records) <- runDB $ do
        l       <- get404 lid
        reviews <- selectList [ReviewLandlord ==. lid] [Desc ReviewCreatedDate]

        let uids = map (reviewReviewer . entityVal) reviews
        users <- selectList [UserId <-. uids] []

        let records = joinTables reviewReviewer reviews users

        return (l,records)

    defaultLayout $ do
        setTitle . T.unpack $ landlordName landlord
        addWidget $(widgetFile "landlord/show")

showGPA ::  [(Entity backend Review, b)] -> String
showGPA = take 4 . show . gpa . map (reviewGrade . entityVal . fst)

reviewWidget :: ReviewId -> Review -> User -> Widget
reviewWidget rid r u = do
    reviewTime <- lift $ liftIO $ humanReadableTime $ reviewCreatedDate r
    $(widgetFile "landlord/_review_row")
