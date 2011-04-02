{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search (getSearchR, postSearchR) where

import Yesod
import Yesod.Markdown
import Renters
import Forms
import Model

import Data.List  (intercalate, partition)
import Data.Maybe (fromJust, isJust, fromMaybe)
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
                    <h1>Reviews for #{landlord}
                    <div .tabdiv>
                        <div .tabcontent>
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
postSearchR = getSearchR
--    addr <- addrFromForm
--
--    let criteria = [ PropertyZipEq (addrZip addr) ] -- zip is mandatory
--            ++ maybeCriteria PropertyAddrOneEq (addrOne addr)
--            ++ maybeCriteria PropertyAddrTwoEq (addrTwo addr)
--            ++ maybeCriteria PropertyCityEq    (addrCity addr)
--            ++ maybeCriteria PropertyStateEq   (addrState addr)
--
--    properties <- return . map snd =<< runDB (selectList criteria [] 0 0)
--    reviews    <- reviewsByProperty properties
--
--    defaultLayout [hamlet|
--        <div .tabdiv>
--            <div .tabcontent>
--                <p>*TODO*
--            |]

showAllReviews :: Handler RepHtml
showAllReviews = do
    reviews <- return . map snd =<< runDB (selectList [] [ReviewCreatedDateDesc] 0 0)
    defaultLayout $ [hamlet|
        <h1>All reviews
        <div .tabdiv>
            <div .tabcontent>
                $forall review <- reviews
                    ^{shortReview review}
        |]

shortReview :: Review -> Widget ()
shortReview review = do
    now       <- lift $ liftIO getCurrentTime
    mreviewer <- lift $ findByKey (reviewReviewer review)
    mproperty <- lift $ findByKey (reviewProperty review)
    content   <- lift . markdownToHtml . Markdown . shorten 200 $ reviewContent review
    
    [hamlet|
        <div .review>
            <div .#{show $ reviewType review}>
                <div .property>
                    $maybe property <- mproperty
                        <p>#{formatProperty property}
                    $nothing
                        <p>No property info...

                <div .content>#{content}
                <div .by>
                    $maybe reviewer <- mreviewer
                        <p>Reviewed by #{reviewerName reviewer} #{humanReadableTimeDiff now $ reviewCreatedDate review}
                    $nothing
                        <p>Reviewed #{humanReadableTimeDiff now $ reviewCreatedDate review}
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
