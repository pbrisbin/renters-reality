module Handler.Search (getSearchR) where

import Import
import Helpers.Grade
import Helpers.Search
import Data.Maybe (fromMaybe)

getSearchR :: Handler RepHtml
getSearchR = do
    query <- runInputGet $ fromMaybe "" <$> iopt (searchField True) "q"

    (results, pageWidget) <-
        if query /= ""
            then executeSearch query
            else return ([], mempty)

    defaultLayout $ do
        setTitle "Search results" 
        $(widgetFile "search")

