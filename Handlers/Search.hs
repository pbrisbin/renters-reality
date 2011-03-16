{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (postSearchR) where

import Yesod
import BadLandlords
import Forms
import Model

postSearchR :: String -> Handler RepHtml
postSearchR "landlord" = do
    landlord <- landlordFromForm
    showComplaints $ LandlordSearch landlord

postSearchR "property" = do
    addr <- addressFromForm
    showComplaints $ PropertySearch addr

postSearchR _ = notFound

showComplaints :: Search -> Handler RepHtml
showComplaints search = do
    complaints <- complaintsBySearch search

    defaultLayout $ do
        setTitle "bad boston landlords | Search complaints"
        [$hamlet|
            <h2>Complaints #{text search}
            <div .tabdiv>
                <div .tabcontent>
                    $forall complaint <- complaints
                        ^{showComplaint complaint}
            |]
    where
        text :: Search -> String
        text (LandlordSearch landlord) = "about " ++ landlordName landlord
        text (PropertySearch addr)     = "at "    ++ formatAddress addr

        formatAddress :: Addr -> String
        formatAddress = const "x"

showComplaint :: Complaint -> Widget ()
showComplaint complaint = undefined
