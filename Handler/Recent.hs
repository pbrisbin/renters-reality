module Handler.Recent (getRecentR) where

import Import
import Yesod.Markdown
import Helpers.Model
import Control.Monad (forM)
import qualified Data.Text as T

getRecentR :: Handler RepJson
getRecentR = do
    records <- runDB $ do
        reviews <- selectList [] [Desc ReviewCreatedDate, LimitTo 5]

        let lids = map (reviewLandlord . entityVal) reviews

        landlords <- selectList [LandlordId <-. lids] []

        return $ joinTables reviewLandlord reviews landlords

    objects <- forM records $ \(review,landlord) -> do
        return $ object [ ("landlord", landlordName $ entityVal landlord)
                        , ("content",  shorten 200 . reviewContent $ entityVal review)
                        , ("link",     "/reviews/" `T.append` (toPathPiece $ entityKey review))
                        ]

    jsonToRepJson $ array objects

    where
        shorten :: Int -> Markdown -> Text
        shorten n (Markdown s) = T.pack $ if length s >= n then take n s ++ "..." else s
