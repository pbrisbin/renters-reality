module Helpers.Search
    ( SearchResult(..)
    , executeSearch
    ) where

import Import
import Database.Persist.Sql (rawSql)
import Text.Shakespeare.Text (st)
import Yesod.Markdown (Markdown(..))
import Yesod.Paginator (paginate)

import qualified Data.Text as T

data SearchResult = SearchResult
    { resId       :: ReviewId
    , resReview   :: Review
    , resLandlord :: Text
    , resExcerpt  :: Html
    }

executeSearch :: Text -> Handler ([SearchResult], Widget)
executeSearch query = do
    records' <- runDB $ rawSql [st|
        SELECT ??, ??
        FROM "Review"
        JOIN "Landlord" ON "Review".landlord = "Landlord".id
        WHERE to_tsvector(name || ' ' || address || ' ' || content) @@ to_tsquery('#{formatQ query}')
        ORDER BY "createdDate" DESC
        |] []

    (records, widget) <- paginate resultLimit records'

    return (map toResult records, widget)

    where
        toResult :: (Entity Review, Entity Landlord) -> SearchResult
        toResult (r, l) =
            let review = entityVal r
                name   = landlordName $ entityVal l
            in SearchResult
                { resId       = entityKey r
                , resReview   = review
                , resLandlord = name
                , resExcerpt  = formatE $ reviewContent review
                }

        formatQ :: Text -> Text
        formatQ = T.intercalate " & " . T.words

        formatE :: Markdown -> Html
        formatE = toHtml . (`T.append` "...") . T.take excerptLimit . unMarkdown

resultLimit :: Int
resultLimit = 25

excerptLimit :: Int
excerptLimit = 250
