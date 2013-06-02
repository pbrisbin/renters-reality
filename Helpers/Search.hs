module Helpers.Search
    ( SearchResults(..)
    , SearchResult(..)
    , executeSearch
    , paginateResults
    ) where

import Import
import Yesod.Paginator (defaultWidget)
import Data.Maybe (fromMaybe)

data SearchResult = SearchResult
    { resId       :: ReviewId
    , resReview   :: Review
    , resLandlord :: Text
    , resExcerpt  :: Html
    }

data SearchResults a = SearchResults
    { searchResults :: [a]
    , searchTotal   :: Int
    , searchPage    :: Int
    , searchQuery   :: Text
    }

executeSearch :: Handler (SearchResults SearchResult)
executeSearch = do
    res <- runInputGet $ (,) <$> iopt (searchField True) "q" <*> iopt intField "p"

    case res of
        -- no search term, show no results
        (Nothing, _) -> return SearchResults
            { searchResults = []
            , searchTotal   = 0
            , searchPage    = 1
            , searchQuery   = ""
            }

        -- search entered, maybe page
        (Just text, mpage) -> do
            let page = fromMaybe 1 mpage

            return SearchResults
                { searchResults = [] -- TODO
                , searchTotal   = 0
                , searchPage    = page
                , searchQuery   = text
                }

paginateResults :: SearchResults SearchResult -> Widget
paginateResults results = do
    let page = searchPage results
        tot  = searchTotal results
        per  = 10

    defaultWidget page per tot
