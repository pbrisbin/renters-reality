module Handler.Recent (getRecentR) where

import Import
import Yesod.Markdown
import Helpers.Model
import Helpers.User
import Control.Monad (forM)
import qualified Data.Text as T

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
        return $ object [ ("landlord",                        landlordName $ entityVal landlord)
                        , ("content" ,         shorten 200 . reviewContent $ entityVal review  )
                        , ("user"    ,                            showName $ entityVal user    )
                        , ("link"    , "/reviews/" `T.append` (toPathPiece $ entityKey review) )
                        ]

    jsonToRepJson $ array objects

    where
        shorten :: Int -> Markdown -> Text
        shorten n (Markdown s) = T.pack $ if length s >= n then take n s ++ "..." else s
