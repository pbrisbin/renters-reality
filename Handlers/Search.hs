{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (getSearchR, postSearchR) where

import Yesod
import Renters
import Forms
import Model

import Data.List  (intercalate, partition)
import Data.Maybe (fromJust, isJust)
import Data.Time  (getCurrentTime)

-- | On a landlord search, the landlord name is a GET param. omitting 
--   this value should display all results
getSearchR :: Handler RepHtml
getSearchR = do
    req <- getRequest
    case getParam req "landlord" of
        Nothing       -> showAllReviews
        Just landlord -> do
            reviews <- reviewsByLandlord $ Landlord landlord
            defaultLayout $ [hamlet|
                    <h2>Reviews for #{landlord}
                    <div .tabdiv>
                        <div .tabcontent>
                            <p>*TODO*
                    |]

-- | On a property search, the address criteria is POSTed. only zip is 
--   mandatory, specifying any other fields just narrows the search, 
--   resulting in a different db select
--
--   todo: Search on f.e. Steet name only? need the sql /like/ keyword
--
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
        <h2>Reviews about #{formatAddr addr}
        <div .tabdiv>
            <div .tabcontent>
                <p>*TODO*
            |]

showAllReviews :: Handler RepHtml
showAllReviews = undefined

shortReview ::  Review -> Widget ()
shortReview review = do
    now       <- lift $ liftIO getCurrentTime
    mproperty <- lift $ findByKey (reviewProperty review)
    case mproperty of
        Nothing       -> return ()
        Just property -> [hamlet|

                       <em>submitted #{humanReadableTimeDiff now $ reviewCreatedDate review}
            |]

formatProperty :: Property -> String
formatProperty p = intercalate ", "
    [ propertyAddrOne p
    , propertyCity    p
    , propertyState   p
    ]

formatAddr :: AddrSearch -> String
formatAddr a = intercalate ", " $ maybeFields ++ [addrZip a]
    where
        maybeFields = map fromJust $ filter isJust
            [ addrOne   a
            , addrCity  a
            , addrState a
            ]

shorten :: Int -> String -> String
shorten n s = if length s > n then take n s ++ "..." else s
