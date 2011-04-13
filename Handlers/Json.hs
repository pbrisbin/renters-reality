{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Json
    ( getJsonLandlordsR
    , getJsonReviewsR
    ) where

import Yesod
import Renters
import Model

import Data.Char  (toLower)
import Data.List  (isInfixOf, intercalate)
import Data.Time  (getCurrentTime)
import Data.Maybe (fromMaybe)

getJsonLandlordsR :: Handler RepJson
getJsonLandlordsR = do
    req       <- getRequest
    landlords <- return . map (landlordName . snd) =<< runDB (selectList [] [LandlordNameAsc] 0 0)

    let results = case getParam req "term" of
            Just term -> filter (isMatch term) landlords
            Nothing   -> landlords

    jsonToRepJson . jsonList $ map jsonScalar results

    where
        isMatch :: String -> String -> Bool
        isMatch x y = (map toLower x) `isInfixOf` (map toLower y)

getJsonReviewsR = do
    req <- getRequest

    let limit = case getParam req "lim" of
            Nothing  -> 0
            Just lim -> read lim :: Int

    reviews <- return . map snd =<< runDB (selectList [] [ReviewCreatedDateDesc] limit 0)

    if null reviews
        then jsonToRepJson $ jsonList []
        else do
            jsonObjects <- mapM jsonReview reviews
            jsonToRepJson $ jsonList jsonObjects

jsonReview :: Review -> Handler Json
jsonReview review = do
    now <- liftIO getCurrentTime

    -- related landlord
    mlandlord <- runDB $ get $ reviewLandlord review
    let landlord = case mlandlord of
            Just landlord' -> landlordName landlord'
            Nothing        -> ""

    -- related property
    mproperty <- runDB $ get $ reviewProperty review
    let property = case mproperty of
            Just property' -> formatProperty property'
            Nothing        -> ""

    -- related reviewer
    mreviewer <- runDB $ get $ reviewReviewer review
    let reviewer = fromMaybe "" $ fmap showName mreviewer

    return $ jsonMap
        [ ("type"     , jsonScalar . formatType                $ reviewType review       )
        , ("created"  , jsonScalar . humanReadableTimeDiff now $ reviewCreatedDate review)
        , ("content"  , jsonScalar                             $ reviewContent review    )
        , ("landlord" , jsonScalar landlord                                              )
        , ("reviewer" , jsonScalar reviewer                                              )
        , ("property" , jsonScalar property                                              )
        ]

formatProperty :: Property -> String
formatProperty p = intercalate ", "
    [ propertyCity p
    , propertyState p
    ]

formatType :: ReviewType -> String
formatType Positive = "positive"
formatType Negative = "negative"
