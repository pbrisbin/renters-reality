{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Landlords (getLandlordsR) where

import Renters
import Model
import Yesod
import Database.Persist.Base
import Yesod.Comments
import Yesod.Goodies
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
            let l = landlord $ head ldocs
            let g = gpa $ map (reviewGrade . review) ldocs
            
            defaultLayout $ do
                Settings.setTitle . T.unpack $ landlordName l
                [hamlet|
                    <div .tabdiv>
                        <div .view-landlord>
                            <div .title>
                                <p>
                                    #{landlordName l}
                                    <span .grade>GPA: #{show $ g}

                            <div .reviews>
                            $forall d <- ldocs
                                ^{listDocument d}
                    |]

noReviews :: Landlord -> Handler RepHtml
noReviews l = defaultLayout $ do
    Settings.setTitle . T.unpack $ landlordName l
    [hamlet|
        <div .tabdiv>
            <div .view-landlord>
                <p>
                    I'm sorry, #{landlordName l} has not been reviewed 
                    yet.

                <p>
                    Would you like to 
                    <a href="@NewR@landlord?#{landlordName l}">write one
                    ?
        |]

listDocument :: Document -> Widget ()
listDocument (Document rid r l u) = do
    reviewTime <- lift . humanReadableTime $ reviewCreatedDate r
    let content = markdownToHtml . shorten 400 $ reviewContent r
    [hamlet|
        <div .review>
            <div .title>
                Reviewed by #{showName u} #{reviewTime}
                <span .grade>
                    #{prettyGrade $ reviewGrade r}

            <div .address>
                #{reviewAddress r}

            <div .review>
                #{content}
        |]

filterDocs :: LandlordId -> [Document] -> [Document]
filterDocs lid = filter ((lEq lid) . reviewLandlord . review)

-- | Somehow related to the persistent upgrade, keys are stored as 
--   PersistInt64 Int64 but when used as a singlePiece they come in as 
--   PersistText Text. This custom eq will ensure that the reviews are 
--   still found
lEq :: LandlordId -> LandlordId -> Bool
lEq a b = a == b || go (unLandlordId a) (unLandlordId b)

    where
        go (PersistText  t) (PersistInt64 i) = t == (T.pack $ show i)
        go (PersistInt64 i) (PersistText  t) = t == (T.pack $ show i)
        go _                _                = False
