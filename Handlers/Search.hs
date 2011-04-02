{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (postSearchR) where

import Yesod
import Renters
import Forms
import Model

import Data.List  (intercalate, partition)
import Data.Maybe (fromJust, isJust)
import Data.Time  (getCurrentTime)

postSearchR :: SearchType -> Handler RepHtml
postSearchR LandlordS = do
    landlord <- landlordFromForm
    reviews  <- reviewsByLandlord landlord
    defaultLayout $ [hamlet|
            <h2>Reviews for #{landlordName landlord}
            <div .tabdiv>
                <div .tabcontent>
                    ^{showReviews LandlordS reviews}
            |]

-- | On a property search, only zip is mandatory, specifying any other 
--   fields just narrows the search, resulting in a different db select
--
--   todo: Search on f.e. Steet name only? need the sql /like/ keyword
--
postSearchR PropertyS = do
    addr <- addrFromForm

    let criteria = [ PropertyZipEq (addrZip addr) ] -- zip is mandatory
            ++ maybeCriteria PropertyAddrOneEq (addrOne addr)
            ++ maybeCriteria PropertyAddrTwoEq (addrTwo addr)
            ++ maybeCriteria PropertyCityEq    (addrCity addr)
            ++ maybeCriteria PropertyStateEq   (addrState addr)

    properties <- return . map snd =<< runDB (selectList criteria [] 0 0)
    reviews    <- reviewsByProperty properties
    defaultLayout [hamlet|
        <h2>Reviews about #{formatAddr addr}
        <div .tabdiv>
            <div .tabcontent>
                ^{showReviews PropertyS reviews}
            |]

showReviews :: SearchType -> [Review] -> Widget ()
showReviews stype reviews = do
    let (good, bad) = partition ((== Positive) . reviewType) reviews
    go Positive good
    go Negative bad
    where
        go :: ReviewType -> [Review] -> Widget ()
        go rtype reviews' = do
            let notnull = not $ null reviews'
            [hamlet|
                <div .#{show rtype}>
                    <h3>#{doShow rtype (length reviews')}:

                    $if notnull
                        <table>
                            <tr>
                                <th>#{dataHeading stype}
                                <th>Review
                                <td>&nbsp;
                            $forall review <- reviews'
                                ^{shortReview stype review}
            |]
            where
                dataHeading :: SearchType -> String
                dataHeading PropertyS = "Landlord"
                dataHeading LandlordS = "Property"

                doShow :: ReviewType -> Int -> String
                doShow Positive 0 = "No positive reviews"
                doShow Positive 1 = "1 positive review"
                doShow Positive n = show n ++ " positive reviews"
                doShow Negative 0 = "No negative reviews"
                doShow Negative 1 = "1 negative review"
                doShow Negative n = show n ++ " negative reviews"

shortReview :: SearchType -> Review -> Widget ()
shortReview LandlordS review = do
    now <- lift $ liftIO getCurrentTime
    mproperty <- lift $ findByKey (reviewProperty review)
    case mproperty of
        Nothing       -> return ()
        Just property -> [hamlet|
            <tr>
                <td>
                    #{formatProperty property}
                <td>
                    <a href="@{ReviewsR $ reviewReference review}"> #{shorten $ reviewContent review}
                <td>
                    <small>
                        <em>submitted #{humanReadableTimeDiff now $ reviewCreatedDate review}
            |]

shortReview PropertyS review = do
    now <- lift $ liftIO getCurrentTime
    mlandlord <- lift $ findByKey (reviewLandlord review)
    case mlandlord of
        Nothing       -> return ()
        Just landlord -> [hamlet|
            <tr>
                <td>
                    #{landlordName landlord}
                <td>
                    <a href="@{ReviewsR $ reviewReference review}"> #{shorten $ reviewContent review}
                <td>
                    <em>submitted #{humanReadableTimeDiff now $ reviewCreatedDate review}
            |]

formatProperty :: Property -> String
formatProperty property = intercalate ", "
    [ propertyAddrOne property
    , propertyCity    property
    , propertyState   property
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
