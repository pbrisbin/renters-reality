module Helpers.Completion
    ( generalCompletion
    , uniqueLandlords
    , uniqueAddresses
    , looseMatch
    ) where

import Import

import Data.List (nub)
import qualified Data.Text as T

generalCompletion :: (Text -> Handler [Text]) -> Handler RepJson
generalCompletion f = do
    mterm <- lookupGetParam "term"
    ss    <- case mterm of
        Nothing   -> return []
        Just ""   -> return []
        Just term -> f term

    return $ RepJson $ toContent $ array ss

uniqueLandlords :: Handler [Text]
uniqueLandlords = do
    lls <- runDB $ selectList [] [Asc LandlordName]
    return $ map (landlordName . entityVal) lls

uniqueAddresses :: Handler [Text]
uniqueAddresses = do
    rs <- runDB $ selectList [] []
    return . nub $ map (formatAddress . entityVal) rs

looseMatch :: Text -> Text -> Bool
looseMatch a b = fix a `T.isInfixOf` fix b

    where
        fix :: Text -> Text
        fix = T.strip . T.toCaseFold
            . T.filter (`notElem` ",.")

formatAddress :: Review -> Text
formatAddress = T.map go . T.filter (/= '\r') . unTextarea . reviewAddress

    where
        go :: Char -> Char
        go '\n' = ' '
        go x    = x
