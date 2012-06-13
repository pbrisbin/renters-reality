module Handler.Recent (getRecentR) where

import Import
import Helpers.Model
import Control.Monad (forM)

getRecentR :: Handler RepJson
getRecentR = do
    records <- runDB $ do
        reviews <- selectList [] [Desc ReviewCreatedDate, LimitTo 10]

        let lids = map (reviewLandlord . entityVal) reviews
        let uids = map (reviewReviewer . entityVal) reviews

        landlords <- selectList [LandlordId <-. lids] []
        users     <- selectList [UserId <-. uids] []

        return $ joinTables3 reviewLandlord reviewReviewer reviews landlords users

    objects <- forM records $ \(review,landlord,user) -> do
        return $ object [ "landlord".= landlord
                        , "review"  .= review
                        , "user"    .= user
                        ]

    jsonToRepJson $ array objects
