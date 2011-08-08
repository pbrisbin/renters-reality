{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Landlords (getLandlordsR) where

import Renters
import Helpers.Widgets
import Yesod.Helpers.RssFeed
import qualified Data.Text as T

getLandlordsR :: Key Landlord -> Handler RepHtml
getLandlordsR lid = do
    docs <- siteDocs =<< getYesod
    let ldocs = docsByLandlord lid docs

    if null ldocs
        then do
            l <- runDB $ get404 lid

            defaultLayout $ do
                setTitle . T.unpack $ landlordName l
                addWidget $(widgetFile "landlords-none")
        else do
            let l  = landlord $ head ldocs
            let tp = (l, ldocs)
            
            defaultLayout $ do
                setTitle . T.unpack $ landlordName l
                rssLink (FeedLandlordR lid) ((++) "rss feed for " . T.unpack $ landlordName l)
                addWidget $(widgetFile "landlords")
