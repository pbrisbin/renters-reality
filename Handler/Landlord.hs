module Handler.Landlord (getLandlordR) where

import Import
import Helpers.Model
import Helpers.User
import Helpers.Grade
import Data.Time.Format.Human
import qualified Data.Text as T

getLandlordR :: LandlordId -> Handler RepHtmlJson
getLandlordR lid = do
    (landlord,records) <- runDB $ do
        l       <- get404 lid
        reviews <- selectList [ReviewLandlord ==. lid] [Desc ReviewCreatedDate]

        let uids = map (reviewReviewer . entityVal) reviews
        users <- selectList [UserId <-. uids] []

        let records = joinTables reviewReviewer reviews users

        return (l,records)


    let jsonRep = object
            [ "landlord" .= (Entity lid landlord)
            , "reviews"  .= (array $ for records $ \(r,u) -> object [ "review" .= r, "user" .= u])
            ]

    let htmlRep = do
        setTitle . T.unpack $ landlordName landlord
        addWidget $(widgetFile "landlord/show")

    defaultLayoutJson htmlRep jsonRep

    where
        for :: [a] -> (a -> b) -> [b]
        for = flip $ map

showGPA ::  [(Entity Review, b)] -> String
showGPA = take 4 . show . gpa . map (reviewGrade . entityVal . fst)

reviewWidget :: ReviewId -> Review -> User -> Widget
reviewWidget rid r user = do
    reviewTime <- lift $ liftIO $ humanReadableTime $ reviewCreatedDate r
    $(widgetFile "landlord/_review_row")
