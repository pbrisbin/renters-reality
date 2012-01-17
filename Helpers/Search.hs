module Helpers.Search where

import Import
import Text.Search.Sphinx
import Text.Search.Sphinx.Types

data SearchForm = SearchForm
    { sfQuery :: Maybe Text
    , sfPage  :: Maybe Int
    }

-- this form is never actually rendered, just used to parse the GET
-- params when the search page is loaded
searchForm :: Form SearchForm
searchForm = renderDivs $ SearchForm
    <$> aopt textField "q" { fsId = Just "q", fsName = Just "q" } Nothing
    <*> aopt intField  "p" { fsId = Just "p", fsName = Just "p" } Nothing

data SearchResult = SearchResult
    { resId       :: ReviewId
    , resLandlord :: Text
    , resExcerpt  :: Widget
    }

searchReviews :: FormResult SearchForm -> Handler ([SearchResult], Widget)
searchReviews = undefined

    --res <- query defaultConfig { port = 9312 } "renters-idx" "brighton"
    --putStrLn $ show res
