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
    landlord   <- landlordFromForm
    complaints <- complaintsByLandlord landlord
    let empty = null complaints
    defaultLayout $ [$hamlet|
            <h2>Complaints for #{landlordName landlord}
            <div .tabdiv>
                <div .tabcontent>
                    $if empty
                        ^{noComplaintsFound}
                    $else
                        <table>
                            <tr>
                                <th>Property
                                <th>Complaint
                                <td>&nbsp;

                            $forall complaint <- complaints
                                ^{shortComplaint complaint}
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
    complaints <- complaintsByProperty properties
    let empty = null complaints
    defaultLayout [$hamlet|
        <h2>Complaints about #{formatAddr addr}
        <div .tabdiv>
            <div .tabcontent>
                $if empty
                    ^{noComplaintsFound}
                $else
                    <table>
                        <tr>
                            <th>Property
                            <th>Complaint
                            <td>&nbsp;

                        $forall complaint <- complaints
                            ^{shortComplaint complaint}
            |]


shortComplaint :: Complaint -> Widget ()
shortComplaint complaint = do
    now <- lift $ liftIO getCurrentTime
    mproperty <- lift $ findByKey (complaintProperty complaint)
    case mproperty of
        Nothing       -> return ()
        Just property -> [$hamlet|
            <tr>
                <td>
                    #{formatProperty property}
                <td>
                    <a href="@{ComplaintsR $ complaintReference complaint}"> #{shorten $ complaintContent complaint}
                <td>
                    <em>submitted #{humanReadableTimeDiff now $ complaintCreatedDate complaint}
            |]

noComplaintsFound :: Widget ()
noComplaintsFound = [$hamlet|
    <p>
        <em>This fish is clean...

    <p>
        I'm sorry, there are no complaints for your search.
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
