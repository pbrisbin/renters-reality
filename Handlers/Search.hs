{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search 
    ( getSearchR
    , getCompLandlordsR
    , getCompSearchesR
    ) where

import Renters
import Helpers.Widgets
import Yesod.Goodies
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

getCompLandlordsR :: Handler RepJson
getCompLandlordsR = generalCompletion $ \t -> do
    landlords <- uniqueLandlords
    return $ filter (looseMatch t) landlords

getCompSearchesR :: Handler RepJson
getCompSearchesR = generalCompletion $ \t -> do
    landlords <- uniqueLandlords
    addrs     <- uniqueAddresses
    return $ filter (looseMatch t) (landlords ++ addrs)

-- | known landlords, formatted for search
uniqueLandlords :: Handler [Text]
uniqueLandlords = return . map (landlordName . snd) =<< runDB (selectList [] [LandlordNameAsc] 0 0)

-- | known addresses, formatted for search
uniqueAddresses :: Handler [Text]
uniqueAddresses = do
    docs <- siteDocs =<< getYesod
    return . nub $ map formatAddress docs

-- | Get the term from the request and pass it to the completion 
--   function, serve the returned values as a list for use in 
--   auto-completion jquery
generalCompletion :: (Text -> Handler [Text]) -> Handler RepJson
generalCompletion f = do
    mterm <- lookupGetParam "term"
    ss    <- case mterm of
        Nothing   -> return []
        Just ""   -> return []
        Just term -> f term

    jsonToRepJson . jsonList $ map (jsonScalar . T.unpack) ss

-- | A loose infix match
looseMatch :: Text -> Text -> Bool
looseMatch a b = fix a `T.isInfixOf` fix b

    where
        fix :: Text -> Text
        fix = T.strip . T.toCaseFold
            . T.filter (`notElem` [',', '.'])

-- | Pagination
myPageOptions :: PageOptions Document Renters Renters
myPageOptions = PageOptions
    { itemsPerPage = 5
    , showItems    = \docs ->
        [hamlet|
            $if null docs
                ^{noReviews}
            $else
                $forall doc <- docs
                    <div .searchresult>
                        ^{landlordGrade doc}
                        ^{reviewContentBlock doc True}
                        ^{reviewedByLink doc}
            |]
    }

getSearchR :: Handler RepHtml
getSearchR = do
    mterm <- lookupGetParam "q"
    docs <- case mterm of
        Nothing   -> siteDocs =<< getYesod
        Just ""   -> siteDocs =<< getYesod
        Just term -> do
            docs' <- siteDocs =<< getYesod
            return $ tryPrefixes term [ ("landlord:", searchByLandlord)
                                      , ("address:" , searchByAddress )
                                      ] fullSearch docs'

    defaultLayout $ do
        setTitle "Search results" 
        addAutoCompletion "#search-input" CompSearchesR
        [hamlet|
            <div .search-form>
                <form .search method="get" action="@{SearchR}">
                   <p>
                       <input #search-input size=30 name="q">
                       <input type="submit" value="Search">

            <div .tabdiv>
                ^{paginate myPageOptions docs}
            |]

-- | handles all sorts of searches
type SearchFunction = Text -> [Document] -> [Document]

-- | wrap in newtype to the correct search instance is used
searchByLandlord :: SearchFunction
searchByLandlord t = map (\(Land d) -> d) . search_ t . map Land

-- | wrap in newtype to the correct search instance is used
searchByAddress :: SearchFunction
searchByAddress t = map (\(Addr d) -> d) . search_ t . map Addr

-- | Uses standard Search instance of Document
fullSearch :: SearchFunction
fullSearch = search_

tryPrefixes :: Text                     -- ^ the search term itself
            -> [(Text, SearchFunction)] -- ^ mappings of prefix -> search function
            -> SearchFunction             -- ^ fall back when nothing matches
            -> [Document] -> [Document]
tryPrefixes term []               fallback docs = fallback term docs
tryPrefixes term ((pref, f):rest) fallback docs = if pref `T.isPrefixOf` term
    then f (T.drop (T.length pref) term) docs
    else tryPrefixes term rest fallback docs

noReviews :: Widget ()
noReviews = [hamlet|
    <p>
        I'm sorry, there are no reviews that meet your search criteria.

    <p>
        Would you like to 
        <a href="@{NewR}">write one
        ?
    |]
