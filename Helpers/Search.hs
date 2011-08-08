{-# LANGUAGE OverloadedStrings #-}
module Helpers.Search 
    ( generalCompletion
    , uniqueLandlords
    , uniqueAddresses
    , looseMatch
    , searchByLandlord
    , searchByAddress
    , fullSearch
    , tryPrefixes
    ) where

import Renters
import Yesod.Goodies.Search
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

generalCompletion :: (Text -> Handler [Text]) -> Handler RepJson
generalCompletion f = do
    mterm <- lookupGetParam "term"
    ss    <- case mterm of
        Nothing   -> return []
        Just ""   -> return []
        Just term -> f term

    jsonToRepJson . jsonList $ map (jsonScalar . T.unpack) ss

uniqueLandlords :: Handler [Text]
uniqueLandlords = return . map (landlordName . snd) =<< runDB (selectList [] [LandlordNameAsc] 0 0)

uniqueAddresses :: Handler [Text]
uniqueAddresses = do
    docs <- siteDocs =<< getYesod
    return . nub $ map formatAddress docs

looseMatch :: Text -> Text -> Bool
looseMatch a b = fix a `T.isInfixOf` fix b

    where
        fix :: Text -> Text
        fix = T.strip . T.toCaseFold
            . T.filter (`notElem` [',', '.'])

type SearchFunction = Text -> [Document] -> [Document]

searchByLandlord :: SearchFunction
searchByLandlord t = map (\(Land d) -> d) . search_ t . map Land

searchByAddress :: SearchFunction
searchByAddress t = map (\(Addr d) -> d) . search_ t . map Addr

fullSearch :: SearchFunction
fullSearch = search_

tryPrefixes :: Text                     -- ^ the search term itself
            -> [(Text, SearchFunction)] -- ^ mappings of prefix -> search function
            -> SearchFunction           -- ^ fall back when nothing matches
            -> [Document] -> [Document]
tryPrefixes term []               fallback docs = fallback term docs
tryPrefixes term ((pref, f):rest) fallback docs = if pref `T.isPrefixOf` term
    then f (T.drop (T.length pref) term) docs
    else tryPrefixes term rest fallback docs
