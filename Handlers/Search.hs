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

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Settings

-- TODO:
autoComplete :: (Document -> [String]) -> Handler [String]
autoComplete f = undefined

-- TODO:
search :: (Document -> Bool) -> Handler (M.Map ReviewId Document)
search p = undefined

getCompLandlordsR :: Handler RepJson
getCompLandlordsR = doComp compLandlords

getCompSearchesR :: Handler RepJson
getCompSearchesR = doComp compSearches

doComp :: (T.Text -> Document -> [String]) -> Handler RepJson
doComp f = do
    mterm   <- lookupGetParam "term"
    results <- case mterm of
        Nothing   -> return []
        Just ""   -> return []
        Just term -> autoComplete (f term)

    jsonToRepJson . jsonList . map jsonScalar $ results

-- TODO:
compLandlords :: T.Text -> Document -> [String]
compLandlords s = undefined

-- TODO:
compSearches :: T.Text -> Document -> [String]
compSearches s = undefined

getSearchR :: Handler RepHtml
getSearchR = do
    mterm <- lookupGetParam "term"
    case mterm of
        Nothing   -> allReviews
        Just ""   -> allReviews
        Just term -> do
            docs <- search (helper term)
            defaultLayout $ do
                Settings.setTitle "Search results" 
                [hamlet|
                    <h1>Search results
                    <div .tabdiv>
                        $if M.null docs
                            ^{noReviews}
                        $else
                            $forall doc <- M.toList docs
                                ^{shortReview doc}
                    |]

    where
        -- TODO:
        helper :: T.Text -> Document -> Bool
        helper = undefined

allReviews :: Handler RepHtml
allReviews = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        Settings.setTitle "All reviews"
        [hamlet|
            <h1>All reviews
            <div .tabdiv>
                $forall doc <- M.toList docs
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

shortReview :: (ReviewId, Document) -> Widget ()
shortReview (rid, (Document landlord property review user)) = do
    let content = markdownToHtml . Markdown . shorten 403 400 $ reviewContent review
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
                        <a href="@{ReviewsR $ rid}">View
        |]
