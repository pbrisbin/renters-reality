module Handler.Search (getSearchR) where

import Import
import Helpers.Search
--import Helpers.Grade

getSearchR :: Handler RepHtml
getSearchR = do
    results <- executeSearch

    let pageWidget = paginateResults results

    defaultLayout $ do
        setTitle "Search results" 
        $(widgetFile "search")

