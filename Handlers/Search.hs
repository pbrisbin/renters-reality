{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (postSearchR) where

import Yesod
import BadLandlords
import Forms
import Model

import Data.List  (intercalate)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Time  (getCurrentTime)

postSearchR :: SearchType -> Handler RepHtml
postSearchR LandlordS = do
    landlord <- landlordFromForm
    reviews  <- reviewsByLandlord landlord
    let empty = null reviews
    defaultLayout $ [$hamlet|
            <h2>Reviews for #{landlordName landlord}
            <div .tabdiv>
                <div .tabcontent>
                    $if empty
                        ^{noReviewsFound}
                    $else
                        <table>
                            <tr>
                                <th>Property
                                <th>Review
                                <td>&nbsp;

                            $forall review <- reviews
                                ^{shortReview review}
            |]

-- | On a property search, on zip is mandatory, specifying any other 
--   fields just narrows the search, resulting in a different db select
postSearchR PropertyS = do
    addr <- addrFromForm
    let criteria = case addr of
            (AddrSearch Nothing Nothing Nothing Nothing zip) -> 
                [ PropertyZipEq zip ]

            (AddrSearch Nothing Nothing Nothing (Just state) zip) ->
                [ PropertyZipEq zip
                , PropertyStateEq state
                ]

            (AddrSearch Nothing Nothing (Just city) (Just state) zip) ->
                [ PropertyZipEq zip
                , PropertyStateEq state
                , PropertyCityEq city
                ]

            (AddrSearch (Just addrOne) Nothing (Just city) (Just state) zip) ->
                [ PropertyZipEq zip
                , PropertyStateEq state
                , PropertyCityEq city
                , PropertyAddrOneEq addrOne
                ]

            (AddrSearch (Just addrOne) (Just addrTwo) (Just city) (Just state) zip) ->
                [ PropertyZipEq zip
                , PropertyStateEq state
                , PropertyCityEq city
                , PropertyAddrOneEq addrOne
                , PropertyAddrTwoEq addrTwo
                ]

    properties <- return . map snd =<< runDB (selectList criteria [] 0 0)
    reviews <- reviewsByProperty properties
    let empty = null reviews
    defaultLayout [$hamlet|
        <h2>Reviews about #{formatAddr addr}
        <div .tabdiv>
            <div .tabcontent>
                $if empty
                    ^{noReviewsFound}
                $else
                    <table>
                        <tr>
                            <th>Property
                            <th>Review
                            <td>&nbsp;

                        $forall review <- reviews
                            ^{shortReview review}
            |]

shortReview :: Review -> Widget ()
shortReview review = do
    now <- lift $ liftIO getCurrentTime
    mproperty <- lift $ findByKey (reviewProperty review)
    case mproperty of
        Nothing       -> return ()
        Just property -> [$hamlet|
            <tr>
                <td>
                    #{formatProperty property}
                <td>
                    <a href="@{ReviewsR $ reviewReference review}"> #{shorten $ reviewContent review}
                <td>
                    <em>submitted #{humanReadableTimeDiff now $ reviewCreatedDate review}
            |]

noReviewsFound :: Widget ()
noReviewsFound = [$hamlet|
    <p>
        <em>This fish is clean...

    <p>
        I'm sorry, there are no reviews for your search.
    |]

formatProperty :: Property -> String
formatProperty property = intercalate ", "
    [ propertyCity    property
    , propertyState   property
    , propertyZip     property
    ]

formatAddr :: AddrSearch -> String
formatAddr a = intercalate ", " $ maybeFields ++ [addrZip a]
    where
        maybeFields = map fromJust $ filter isJust
            [ addrOne   a
            , addrTwo   a
            , addrCity  a
            , addrState a
            ]

shorten :: String -> String
shorten s = if length s > 30 then take 30 s ++ "..." else s
