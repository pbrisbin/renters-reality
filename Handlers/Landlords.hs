{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Landlords (getLandlordsR) where

import Renters
import Model
import Yesod
import Helpers.Widgets
import Database.Persist.Base
import qualified Data.Text as T
import qualified Settings

getLandlordsR :: Key Landlord -> Handler RepHtml
getLandlordsR lid = do
    docs <- siteDocs =<< getYesod
    let ldocs = filterDocs lid docs

    -- show the no reviews page, 404 if ll doesn't exist at all
    if null ldocs
        then noReviews =<< runDB (get404 lid)
        else do
            -- known to be safe
            let l  = landlord $ head ldocs
            let tp = (l, ldocs)
            
            defaultLayout $ do
                Settings.setTitle . T.unpack $ landlordName l
                [hamlet|
                    <div .tabdiv>
                        <div .view-landlord>
                            ^{landlordGPA tp}
                            $forall d <- ldocs
                                <div .review>
                                    ^{reviewedByGrade d}
                                    ^{reviewContentBlock d True}
                                    <a href=@{ReviewsR $ reviewId d}>Read more...
                    |]

noReviews :: Landlord -> Handler RepHtml
noReviews l = defaultLayout $ do
    Settings.setTitle . T.unpack $ landlordName l
    [hamlet|
        <div .tabdiv>
            <p>
                I'm sorry, #{landlordName l} has not been reviewed 
                yet.

            <p>
                Would you like to 
                <a href="@NewR@landlord?#{landlordName l}">write one
                ?
        |]

filterDocs :: LandlordId -> [Document] -> [Document]
filterDocs lid = filter ((lEq lid) . reviewLandlord . review)

    where
        lEq :: LandlordId -> LandlordId -> Bool
        lEq a b = a == b || go (unLandlordId a) (unLandlordId b)

        go (PersistText  t) (PersistInt64 i) = t == (T.pack $ show i)
        go (PersistInt64 i) (PersistText  t) = t == (T.pack $ show i)
        go _                _                = False
