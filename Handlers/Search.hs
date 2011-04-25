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
import qualified Settings

getCompLandlordsR :: Handler RepJson
getCompLandlordsR = jsonToRepJson . jsonList $ map jsonScalar []

getCompSearchesR :: Handler RepJson
getCompSearchesR = jsonToRepJson . jsonList $ map jsonScalar []

getSearchR :: Handler RepHtml
getSearchR = do
    mterm <- lookupGetParam "term"
    case mterm of
        Nothing   -> allReviews
        Just ""   -> allReviews
        Just term -> do
            docs <- siteDocs =<< getYesod
            defaultLayout $ do
                Settings.setTitle "Search results" 
                [hamlet|
                    <h1>Search results
                    <div .tabdiv>
                        $if null docs
                            ^{noReviews}
                        $else
                            $forall doc <- docs
                                ^{shortReview doc}
                    |]

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
    let content = markdownToHtml . liftMD (shorten' 400) $ reviewContent review
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

    where
        liftMD :: (String -> String) -> Markdown -> Markdown
        liftMD f (Markdown s) = Markdown $ f s
