module Handler.User (getUserR) where

import Import
import Helpers.Model
import Helpers.User
import Helpers.Grade
import Data.Time.Format.Human
import qualified Data.Text as T

getUserR :: UserId -> Handler RepHtml
getUserR uid = do
    muid <- maybeAuth

    (user,records) <- runDB $ do
        u       <- get404 uid
        reviews <- selectList [ReviewReviewer ==. uid] [Desc ReviewCreatedDate]

        let lids = map (reviewLandlord . entityVal) reviews
        landlords <- selectList [LandlordId <-. lids] []

        let records = joinTables reviewLandlord reviews landlords

        return (u,records)

    defaultLayout $ do
        setTitle . T.unpack $ showName user
        addWidget $(widgetFile "user/show")

reviewWidget :: ReviewId -> Review -> Landlord -> Widget
reviewWidget rid r l = do
    reviewTime <- lift $ liftIO $ humanReadableTime $ reviewCreatedDate r
    $(widgetFile "user/_review_row")
