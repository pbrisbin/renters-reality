{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search 
    ( getSearchR
    , getCompLandlordsR
    , getCompSearchesR
    ) where

import Renters
import Model
import Yesod
import Yesod.Comments.Markdown
import Control.Monad (forM)
import qualified Data.Text as T
import qualified Settings

getCompLandlordsR :: Handler RepJson
getCompLandlordsR = do
    mterm <- lookupGetParam "term"
    ss    <- case mterm of
        Just term -> do
            res <- runDB $ selectList [] [LandlordNameAsc] 0 0
            forM res $ \(k, (Landlord name)) -> do
                return $ if term `looseMatch` name then [name] else []

        _ -> return []

    jsonToRepJson . jsonList . map (jsonScalar . T.unpack) $ concat ss

getCompSearchesR :: Handler RepJson
getCompSearchesR = do
    mterm <- lookupGetParam "term"
    ss    <- case mterm of
        Just term -> do
            resL <- runDB $ selectList [] [LandlordNameAsc] 0 0
            resP <- runDB $ selectList [] [PropertyZipAsc]  0 0

            -- autocomplete landlord names
            landSS <- forM resL $ \(k, (Landlord name)) -> do
                return $ if term `looseMatch` name then [name] else []

            -- autocomplete property strings
            propSS <- forM resP $ \(k, p) -> do
                return $ if term `looseMatch` formatProperty p then [formatProperty p] else []

            return $ landSS ++ propSS

        _ -> return []

    jsonToRepJson . jsonList $ map (jsonScalar . T.unpack) $ concat ss

getSearchR :: Handler RepHtml
getSearchR = do
    mterm <- lookupGetParam "term"
    case mterm of
        Nothing   -> allReviews
        Just ""   -> allReviews
        Just term -> do
            docs <- siteDocs =<< getYesod
            let filtered = filter (helper term) docs
            defaultLayout $ do
                Settings.setTitle "Search results" 
                [hamlet|
                    <h1>Search results
                    <div .tabdiv>
                        $if null filtered
                            ^{noReviews}
                        $else
                            $forall doc <- filtered
                                ^{shortReview doc}
                    |]

    where
        helper :: T.Text -> Document -> Bool
        helper term (Document _ _ l p _) = term `looseMatch` landlordName l ||
                                           term `looseMatch` formatProperty p

allReviews :: Handler RepHtml
allReviews = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        Settings.setTitle "All reviews"
        [hamlet|
            <h1>All reviews
            <div .tabdiv>
                $forall doc <- docs
                    ^{shortReview doc}
            |]

noReviews :: Widget ()
noReviews = [hamlet|
    <p>
        I'm sorry, there are no reviews that meet your search criteria.

    <p>
        Would you like to 
        <a href="@{NewR Positive}">write 
        <a href="@{NewR Negative}">one
        ?
    |]

shortReview :: Document -> Widget ()
shortReview (Document rid review landlord property user) = do
    let content = markdownToHtml . shorten 400 $ reviewContent review
    reviewTime <- lift . humanReadableTimeDiff $ reviewCreatedDate review
    
    [hamlet|
        <div .review>
            <div .#{show $ reviewType review}>
                <div .property>
                    <p>#{landlordName landlord} - #{formatProperty property}
                <div .content>
                    #{content}
                <div .by>
                    <p>
                        Reviewed by #{showName user} #{reviewTime} 
                        <span .view-link>
                            <a href="@{ReviewsR $ rid}">View
        |]
