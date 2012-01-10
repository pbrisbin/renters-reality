{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Search 
    ( getSearchR
    , getCompLandlordsR
    , getCompSearchesR
    ) where

import Foundation
import Helpers.Search
import Yesod.Goodies

getSearchR :: Handler RepHtml
getSearchR = do
    mterm <- lookupGetParam "q"
    docs' <- case mterm of
        Nothing   -> siteDocs =<< getYesod
        Just ""   -> siteDocs =<< getYesod
        Just term -> do
            docs'' <- siteDocs =<< getYesod
            return $ tryPrefixes term [ ("landlord:", searchByLandlord)
                                      , ("address:" , searchByAddress )
                                      ] fullSearch docs''

    (docs, pageWidget) <- paginate 5 docs'

    defaultLayout $ do
        setTitle "Search results" 
        addWidget $(widgetFile "search")


getCompLandlordsR :: Handler RepJson
getCompLandlordsR = generalCompletion $ \t -> do
    landlords <- uniqueLandlords
    return $ filter (looseMatch t) landlords

getCompSearchesR :: Handler RepJson
getCompSearchesR = generalCompletion $ \t -> do
    landlords <- uniqueLandlords
    addrs     <- uniqueAddresses
    return $ filter (looseMatch t) (landlords ++ addrs)
