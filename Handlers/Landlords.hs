{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Landlords (getLandlordsR) where

import Renters
import Yesod.Helpers.RssFeed
import qualified Data.Text as T

getLandlordsR :: Key Landlord -> Handler RepHtml
getLandlordsR lid = do
    docs <- siteDocs =<< getYesod

    let ldocs = docsByLandlordId lid docs
    let none  = null ldocs -- no reviews?

    l <- if none
            then runDB $ get404 lid
            else return . landlord $ head ldocs

    let tp = (l, ldocs)

    defaultLayout $ do
        setTitle . T.unpack $ landlordName l
        rssLink (FeedLandlordR lid) ((++) "rss feed for " . T.unpack $ landlordName l)
        addWidget $(widgetFile "landlord")
