{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Json (getJsonR) where

import Yesod
import BadLandlords
import Model

import Data.List (isInfixOf)

-- todo: case-insensitive searches
getJsonR :: JsonSearch -> Handler RepJson
getJsonR LandlordJ = do
    params <- return . reqGetParams =<< getRequest
    let term = getParam "term" params

    landlords' <- return . map (landlordName . snd) =<< runDB (selectList [] [LandlordNameAsc] 0 0)
    let landlords = if term /= [] then filter (isInfixOf term) landlords' else landlords'
    jsonToRepJson . jsonList $ map jsonScalar landlords
    where
        -- poor man's lookup
        getParam :: String -> [(String,String)] -> String
        getParam k []            = []
        getParam k ((k',v):rest) = if k == k' then v else getParam k rest
