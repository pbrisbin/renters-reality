{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Complaints 
    ( getComplaintsR
    , postComplaintsR -- comments
    ) where

import Yesod
import BadLandlords
import Model

import Data.List (intercalate)
import Data.Time (getCurrentTime)

getComplaintsR :: Int -> Handler RepHtml
getComplaintsR ref = do
    mcomplaint <- runDB $ getBy (UniqueComplaint ref)
    case mcomplaint of
        Nothing             -> notFound
        Just (k, complaint) -> do
            mlandlord   <- findByKey (complaintLandlord   complaint)
            mproperty   <- findByKey (complaintProperty   complaint)
            mcomplainer <- findByKey (complaintComplainer complaint)
            case (mlandlord,mproperty,mcomplainer) of
                (Just landlord, Just property, Just complainer) -> do
                    now <- liftIO getCurrentTime
                    defaultLayout $ do
                        setTitle "bad boston landlords | View complaint"
                        [$hamlet|
                            <h2>Complaint ##{show ref}
                            <div .tabdiv>
                                <div .tabcontent>
                                    <h3>
                                        #{landlordName landlord}
                                        \ - #{formatProperty property}

                                    <p>
                                        Submitted #{humanReadableTimeDiff now $ complaintCreatedDate complaint} 
                                        by #{formatComplainer complainer}

                                    <p>
                                        <strong>Complaint:

                                    <blockquote>
                                        #{Textarea $ complaintContent complaint}
                            |]

                _ -> notFound

postComplaintsR :: Int -> Handler RepHtml
postComplaintsR = getComplaintsR

formatProperty :: Property -> String
formatProperty p = intercalate ", "
    [ propertyAddrOne p
    , propertyAddrTwo p
    , propertyCity    p
    , propertyState   p
    , propertyZip     p
    ]

formatComplainer :: Complainer -> String
formatComplainer c = complainerName c ++ " <" ++ complainerEmail c ++ ">"
