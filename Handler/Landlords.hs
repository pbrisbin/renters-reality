module Handler.Landlords (getLandlordsR) where

import Import
import Control.Monad (forM)

getLandlordsR :: Handler RepHtml
getLandlordsR = do
    records <- runDB $ do
        landlords <- selectList [] [Asc LandlordName]
        forM landlords $ \landlord -> do
            cnt <- count [ReviewLandlord ==. entityKey landlord]

            return (landlord,cnt)

    defaultLayout $ do
        setTitle "All landlords"
        addWidget $(widgetFile "landlord/index")

landlordWidget :: LandlordId -> Landlord -> Int -> Widget
landlordWidget lid l c = $(widgetFile "landlord/_landlord_row")
