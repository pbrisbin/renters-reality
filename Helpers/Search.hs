module Helpers.Search
    ( SearchResults(..)
    , SearchResult(..)
    , executeSearch
    , paginateResults
    ) where

import Import
import Yesod.Paginator (paginationWidget)
import Helpers.Sphinx
import Settings (SphinxSettings(..), sphinxSettings)

import Yesod.Markdown
import Database.Persist.Store (PersistValue(..))
import qualified Data.Text as T

data SearchResult = SearchResult
    { resId       :: ReviewId
    , resReview   :: Review
    , resLandlord :: Text
    , resExcerpt  :: Html
    }

executeSearch :: Handler (SearchResults SearchResult)
executeSearch = executeQuery
    (sphinxIndex   sphinxSettings)
    (sphinxPort    sphinxSettings)
    (sphinxPerPage sphinxSettings)
    $ \text match -> do
        let rid = Key . PersistInt64 $ documentId match

        runDB $ do
            mreview <- get rid
            case mreview of
                Just r -> do
                    mlandlord <- get (reviewLandlord r)
                    case mlandlord of
                        Just l -> do
                            excerpt <- liftIO $ mkExcerpt (reviewContent r) text
                            return $ Just $ SearchResult
                                                { resId       = rid 
                                                , resReview   = r
                                                , resLandlord = (landlordName l)
                                                , resExcerpt  = excerpt
                                                }

                        _ -> return Nothing

                _ -> return Nothing

mkExcerpt :: Markdown -> Text -> IO Html
mkExcerpt (Markdown s) qstring = buildExcerpt (sphinxIndex sphinxSettings) (sphinxPort  sphinxSettings) s (T.unpack qstring)

paginateResults :: SearchResults SearchResult -> Widget
paginateResults results = do
    let page = searchPage results
        tot  = searchTotal results
        per  = sphinxPerPage sphinxSettings

    paginationWidget page per tot
