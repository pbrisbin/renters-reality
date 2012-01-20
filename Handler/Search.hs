module Handler.Search (getSearchR) where

import Import
import Helpers.Search
import Helpers.Grade

getSearchR :: Handler RepHtml
getSearchR = do
    ((results,pageWidget),res) <- executeSearch matchToResult

    defaultLayout $ do
        setTitle "Search results" 
        addWidget $(widgetFile "search")

curQuery :: FormResult SearchForm -> Text
curQuery (FormSuccess (SearchForm (Just q) _)) = q
curQuery _                                     = ""

curPage :: FormResult SearchForm -> Int
curPage (FormSuccess (SearchForm _ (Just p))) = p
curPage _                                     = 1
