{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Json (getJsonR) where

import Yesod
import BadLandlords
import Model

import Data.Char (toLower)
import Data.List (isInfixOf)

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
