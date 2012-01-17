module Handler.Completion
    ( getCompLandlordsR
    , getCompSearchesR
    ) where

import Import
import Helpers.Completion

getCompLandlordsR :: Handler RepJson
getCompLandlordsR = generalCompletion $ \t -> do
    landlords <- uniqueLandlords
    return $ filter (looseMatch t) landlords

getCompSearchesR :: Handler RepJson
getCompSearchesR = generalCompletion $ \t -> do
    landlords <- uniqueLandlords
    addrs     <- uniqueAddresses
    return $ filter (looseMatch t) (landlords ++ addrs)
