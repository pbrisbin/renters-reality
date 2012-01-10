{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Landlords (getLandlordsR) where

import Foundation
import Helpers.Search
import Yesod.Goodies
import Yesod.RssFeed (rssLink)
import qualified Data.Text as T

getLandlordsR :: LandlordId -> Handler RepHtml
getLandlordsR lid = do
    -- all documents
    docs'' <- siteDocs =<< getYesod

    -- filter by landlord
    let docs' = docsByLandlordId lid docs''
    let none  = null docs' -- no reviews?

    l <- if none
            then runDB $ get404 lid
            else return . landlord $ head docs'

    -- paginated
    (docs, pageWidget) <- paginate 5 docs'

    defaultLayout $ do
        setTitle . T.unpack $ landlordName l
        rssLink (FeedLandlordR lid) ((++) "rss feed for " . T.unpack $ landlordName l)
        addWidget $(widgetFile "landlord/show")

gpa' :: [Document] -> Double
gpa' = gpa . map (reviewGrade . review)
