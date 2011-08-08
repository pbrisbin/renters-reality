{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search 
    ( getSearchR
    , getCompLandlordsR
    , getCompSearchesR
    ) where

import Renters
import Helpers.Search
import Helpers.Widgets
import Yesod.Goodies.Paginate

getSearchR :: Handler RepHtml
getSearchR = do
    mterm <- lookupGetParam "q"
    docs <- case mterm of
        Nothing   -> siteDocs =<< getYesod
        Just ""   -> siteDocs =<< getYesod
        Just term -> do
            docs' <- siteDocs =<< getYesod
            return $ tryPrefixes term [ ("landlord:", searchByLandlord)
                                      , ("address:" , searchByAddress )
                                      ] fullSearch docs'

    defaultLayout $ do
        setTitle "Search results" 
        addWidget $(widgetFile "search")
        addAutoCompletion "#search-input" CompSearchesR

    where
        myPageOptions :: PageOptions Document Renters Renters
        myPageOptions = PageOptions
            { itemsPerPage = 5
            , showItems    = \docs -> addWidget $(widgetFile "_showitems")
            }

getCompLandlordsR :: Handler RepJson
getCompLandlordsR = generalCompletion $ \t -> do
    landlords <- uniqueLandlords
    return $ filter (looseMatch t) landlords

getCompSearchesR :: Handler RepJson
getCompSearchesR = generalCompletion $ \t -> do
    landlords <- uniqueLandlords
    addrs     <- uniqueAddresses
    return $ filter (looseMatch t) (landlords ++ addrs)
