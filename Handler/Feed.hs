module Handler.Feed 
    ( getFeedR
    , getFeedLandlordR
    ) where

import Import
import Yesod -- TODO
import Helpers.Grade
import Helpers.Model

import Yesod.Markdown
import Yesod.RssFeed
import qualified Data.Text as T

getFeedR :: Handler RepRss
getFeedR = do
    records <- runDB $ do
        reviews <- selectList [] [Desc ReviewCreatedDate, LimitTo 10]

        let lids = map (reviewLandlord . entityVal) reviews
        landlords <- selectList [LandlordId <-. lids] []

        return $ joinTables reviewLandlord reviews landlords

    feedFromRecords records

getFeedLandlordR :: LandlordId -> Handler RepRss
getFeedLandlordR lid = do
    records <- runDB $ do
        landlord <- get404 lid
        reviews  <- selectList [ReviewLandlord ==. lid] [Desc ReviewCreatedDate, LimitTo 10]

        return $ zip reviews (repeat $ Entity lid landlord)

    feedFromRecords records

feedFromRecords :: [(Entity Review, Entity Landlord)] -> Handler RepRss
feedFromRecords [] = notFound
feedFromRecords records@(r:_) =
    rssFeed Feed
        { feedAuthor      = "Patrick Brisbin"
        , feedTitle       = "Renters' reality"
        , feedDescription = "Recent reviews on rentersreality.com"
        , feedLanguage    = "en-us"
        , feedLinkSelf    = FeedR
        , feedLinkHome    = RootR
        , feedUpdated     = reviewCreatedDate . entityVal $ fst r
        , feedEntries     = map recordToRssEntry records
        }

recordToRssEntry :: (Entity Review, Entity Landlord) -> FeedEntry (Route App)
recordToRssEntry (Entity rid r, Entity _ l) =
    FeedEntry
        { feedEntryLink    = ReviewR rid
        , feedEntryUpdated = reviewCreatedDate r
        , feedEntryTitle   = mkTitle (landlordName l) (reviewGrade r)
        , feedEntryContent = plainText $ reviewContent r
        }

    where
        mkTitle :: Text -> Grade -> Text
        mkTitle n g = n `T.append` " reviewed as "
                        `T.append` prettyGrade g
                        `T.append` " on rentersreality.com"

        plainText :: Markdown -> Html
        plainText (Markdown s) = toHtml s
