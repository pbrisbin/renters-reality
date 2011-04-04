{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (getSearchR, postSearchR) where

import Yesod
import Yesod.Markdown

import Renters
import Model

import qualified Settings

import Control.Applicative ((<$>),(<*>))
import Data.List           (intercalate, partition)
import Data.Maybe          (fromJust, isJust, fromMaybe)
import Data.Time           (getCurrentTime)

data AddrSearch = AddrSearch
    { addrOne   :: Maybe String
    , addrTwo   :: Maybe String
    , addrCity  :: Maybe String
    , addrState :: Maybe String
    , addrZip   :: String
    }

-- | On a landlord search, the landlord name is a GET param. omitting 
--   this value should display all results
getSearchR :: Handler RepHtml
getSearchR = do
    req <- getRequest
    case getParam req "landlord" of
        Nothing       -> showAllReviews
        Just ""       -> showAllReviews
        Just landlord -> do
            reviews <- reviewsByLandlord $ Landlord landlord
            defaultLayout $ do
                Settings.setTitle "Search" 
                [hamlet|
                    <h1>Reviews for #{landlord}
                    <div .tabdiv>
                        <div .tabcontent>
                            $if null reviews
                                ^{noneFound}
                            $else
                                $forall review <- reviews
                                    ^{shortReview review}
                    |]

-- | On a property search, the address criteria is POSTed. only zip is 
--   mandatory, specifying any other fields just narrows the search, 
--   resulting in a different db select
--
--   todo: Search on f.e. Steet name only? need the sql /like/ keyword
--
postSearchR :: Handler RepHtml
postSearchR = do
    addr <- addrFromForm

    let criteria = [ PropertyZipEq (addrZip addr) ] -- zip is mandatory
            ++ maybeCriteria PropertyAddrOneEq (addrOne addr)
            ++ maybeCriteria PropertyAddrTwoEq (addrTwo addr)
            ++ maybeCriteria PropertyCityEq    (addrCity addr)
            ++ maybeCriteria PropertyStateEq   (addrState addr)

    properties <- return . map snd =<< runDB (selectList criteria [] 0 0)
    reviews    <- reviewsByProperty properties

    defaultLayout [hamlet|
        <h1>Reviews by area
        <div .tabdiv>
            <div .tabcontent>
                $if null reviews
                    ^{noneFound}
                $else
                    $forall review <- reviews
                        ^{shortReview review}
            |]

    where
        addrFromForm :: Handler AddrSearch
        addrFromForm = runFormPost' $ AddrSearch
            <$> maybeStringInput "addrone"
            <*> maybeStringInput "addrtwo"
            <*> maybeStringInput "city"
            <*> maybeStringInput "state"
            <*> stringInput "zip"


showAllReviews :: Handler RepHtml
showAllReviews = do
    reviews <- return . map snd =<< runDB (selectList [] [ReviewCreatedDateDesc] 0 0)
    defaultLayout $ do
        Settings.setTitle "Search"
        [hamlet|
            <h1>All reviews
            <div .tabdiv>
                <div .tabcontent>
                    $forall review <- reviews
                        ^{shortReview review}
            |]

noneFound :: Widget ()
noneFound = [hamlet|
    <p>
        I'm sorry, there are no reviews that meet your 
        search criteria.

    <p>
        Would you like to 
        <a href="@{NewR Positive}">write 
        <a href="@{NewR Negative}">one
        ?
    |]

shortReview :: Review -> Widget ()
shortReview review = do
    now       <- lift $ liftIO getCurrentTime
    mreviewer <- lift $ findByKey (reviewReviewer review)
    mproperty <- lift $ findByKey (reviewProperty review)
    mlandlord <- lift $ findByKey (reviewLandlord review)

    let content = markdownToHtml . Markdown . shorten 400 $ reviewContent review
    
    [hamlet|
        <div .review>
            <div .#{show $ reviewType review}>
                <div .property>
                    $maybe property <- mproperty
                        <p>#{maybe "No landlord info" landlordName mlandlord} - #{formatProperty property}
                    $nothing
                        <p>#{maybe "No landlord info" landlordName mlandlord} - No property info

                <div .content>#{content}

                <div .by>
                    $maybe reviewer <- mreviewer
                        <p>
                            Reviewed by #{reviewerName reviewer} #{humanReadableTimeDiff now $ reviewCreatedDate review}. 
                            <a href="@{ReviewsR $ reviewReference review}">View
                    $nothing
                        <p>
                            Reviewed #{humanReadableTimeDiff now $ reviewCreatedDate review}. 
                            <a href="@{ReviewsR $ reviewReference review}">View
        |]

formatProperty :: Property -> String
formatProperty p = intercalate ", "
    [ propertyAddrOne p
    , propertyCity    p
    , propertyState   p
    ]

shorten :: Int -> String -> String
shorten n s = if length s > n then take n s ++ "..." else s
