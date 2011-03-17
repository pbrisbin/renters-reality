{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (postSearchR) where

import Yesod
import BadLandlords
import Forms
import Model

import Data.List  (intercalate)
import Data.Maybe (fromMaybe)
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
    <p>I'm sorry, there are no complaints for your search.
    <p>
        Would you like to 
        <a href="#">create one
        ?
    |]

formatProperty :: Property -> String
formatProperty property = intercalate ", "
    [ propertyCity    property
    , propertyState   property
    , propertyZip     property
    ]

shorten :: String -> String
shorten s = if length s > 30 then take 30 s ++ "..." else s
