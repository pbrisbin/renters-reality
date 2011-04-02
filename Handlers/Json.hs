{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Json (getJsonR) where

import Yesod
import Renters
import Model

import Data.Char (toLower)
import Data.List (isInfixOf, intercalate)
import Data.Time (getCurrentTime)

-- todo: case-insensitive searches
getJsonR :: JsonSearch -> Handler RepJson
getJsonR LandlordJ = do
    req       <- getRequest
    landlords <- return . map (landlordName . snd) =<< runDB (selectList [] [LandlordNameAsc] 0 0)

    let results = case getParam req "term" of
            Just term -> filter (isMatch term) landlords
            Nothing   -> landlords

    jsonToRepJson . jsonList $ map jsonScalar results

    where
        isMatch :: String -> String -> Bool
        isMatch x y = (map toLower x) `isInfixOf` (map toLower y)

getJsonR ReviewsJ = do
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
    mlandlord <- findByKey $ reviewLandlord review
    let landlord = case mlandlord of
            Just landlord' -> landlordName landlord'
            Nothing        -> ""

    -- related property
    mproperty <- findByKey $ reviewProperty review
    let property = case mproperty of
            Just property' -> formatProperty property'
            Nothing        -> ""

    -- related reviewer
    mreviewer <- findByKey $ reviewReviewer review
    let reviewer = case mreviewer of
            Just reviewer' -> reviewerName reviewer'
            Nothing        -> ""

    return $ jsonMap
        [ ("reference", jsonScalar . show                      $ reviewReference review  )
        , ("type"     , jsonScalar . formatType                $ reviewType review       )
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
