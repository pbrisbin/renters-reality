{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (getSearchR) where

import Yesod
import Yesod.Markdown

import Renters
import Model hiding (shorten)

import Control.Monad       (forM)
import Data.Time           (getCurrentTime)
import Data.List           (nubBy)

import qualified Settings

getSearchR :: Handler RepHtml
getSearchR = do
    req <- getRequest
    case getParam req "term" of
        Nothing   -> allReviews
        Just ""   -> allReviews
        Just term -> do
            -- search both ways
            landlords  <- (search term :: Handler [(Key Landlord, Landlord)])
            properties <- (search term :: Handler [(Key Property, Property)])

            reviews <- do
                revsL <- forM landlords  $ \(k,_) -> runDB (selectList [ReviewLandlordEq k] [] 0 0)
                revsP <- forM properties $ \(k,_) -> runDB (selectList [ReviewPropertyEq k] [] 0 0)
                return . mkUnique . concat $ revsL ++ revsP
            
            defaultLayout $ do
                Settings.setTitle "Search results" 
                [hamlet|
                    <h1>Search results
                    <div .tabdiv>
                        $if null reviews
                            ^{noReviews}
                        $else
                            $forall review <- reviews
                                ^{shortReview review}
                    |]

mkUnique :: [(ReviewId, Review)] -> [(ReviewId, Review)]
mkUnique = nubBy (\a b -> fst a == fst b)

allReviews :: Handler RepHtml
allReviews = do
    reviews <- runDB $ selectList [] [ReviewCreatedDateDesc] 0 0
    defaultLayout $ do
        Settings.setTitle "All reviews"
        [hamlet|
            <h1>All reviews
            <div .tabdiv>
                $forall review <- reviews
                    ^{shortReview review}
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

shortReview :: (ReviewId, Review) -> Widget ()
shortReview (rid, review) = do
    now       <- lift $ liftIO getCurrentTime
    mreviewer <- lift $ runDB $ get $ reviewReviewer review
    mproperty <- lift $ runDB $ get $ reviewProperty review
    mlandlord <- lift $ runDB $ get $ reviewLandlord review

    let content = markdownToHtml . Markdown . shorten 400 $ reviewContent review
    
    [hamlet|
        <div .review>
            <div .#{show $ reviewType review}>
                <div .property>
                    <p>#{maybe "No landlord info" landlordName mlandlord} - #{maybe "No property info" formatProperty mproperty}
                <div .content>
                    #{content}
                <div .by>
                    <p>
                        Reviewed by #{maybe "anonymous" showName mreviewer} #{humanReadableTimeDiff now $ reviewCreatedDate review}. 
                        <a href="@{ReviewsR $ rid}">View
        |]

shorten :: Int -> String -> String
shorten n s = if length s > n then take n s ++ "..." else s
