module Helpers.Search 
    ( generalCompletion
    , uniqueLandlords
    , uniqueAddresses
    , searchByLandlord
    , searchByAddress
    , fullSearch
    , tryPrefixes
    , looseMatch
    , asSearchResult
    ) where

import Import

import Yesod.Goodies
import Data.List     (nub)
import Data.Ord      (comparing)
--import Database.Persist.Store (Entity(..))
import qualified Data.Text as T

type SearchFunction = Text -> [Document] -> [Document]

-- | Look for prefixes in the search term, if found use the mapped 
--   search function for that term. if none are found, use the fallback 
--   function.
tryPrefixes :: Text                     -- ^ the search term itself
            -> [(Text, SearchFunction)] -- ^ mappings of prefix -> search function
            -> SearchFunction           -- ^ fall back when nothing matches
            -> [Document] -> [Document]
tryPrefixes term []               fallback docs = fallback term docs
tryPrefixes term ((pref, f):rest) fallback docs = if pref `T.isPrefixOf` term
    then f (T.drop (T.length pref) term) docs
    else tryPrefixes term rest fallback docs

-- | to support "landlord: foo" and "address: bar" type searches, we'll 
--   create some newtypes that wrap document, then make instances of 
--   Search for them which only search the piece defined
newtype LDoc = Land Document -- ^ search just landlord
newtype ADoc = Addr Document -- ^ search just addresses

instance TextSearch LDoc where
    toText (Land (Document _ _ l _)) = landlordName l

instance TextSearch ADoc where
    toText (Addr d) = formatAddress d

instance Search LDoc where
    preference = comparing (reviewCreatedDate . review . (\(Land d) -> d) . searchResult)
    match      = keywordMatch

instance Search ADoc where
    preference = comparing (reviewCreatedDate . review . (\(Addr d) -> d) . searchResult)
    match      = keywordMatch

searchByLandlord :: SearchFunction
searchByLandlord t = map (\(Land d) -> d) . search_ t . map Land

searchByAddress :: SearchFunction
searchByAddress t = map (\(Addr d) -> d) . search_ t . map Addr

fullSearch :: SearchFunction
fullSearch = search_

uniqueLandlords :: Handler [Text]
uniqueLandlords = do
    return []
    --lls <- runDB $ selectList [] [Asc LandlordName]
    --return $ map (landlordName . entityVal) lls

uniqueAddresses :: Handler [Text]
uniqueAddresses = do
    docs <- siteDocs =<< getYesod
    return . nub $ map formatAddress docs

generalCompletion :: (Text -> Handler [Text]) -> Handler RepJson
generalCompletion f = do
    mterm <- lookupGetParam "term"
    ss    <- case mterm of
        Nothing   -> return []
        Just ""   -> return []
        Just term -> f term

    jsonToRepJson $ array ss

looseMatch :: Text -> Text -> Bool
looseMatch a b = fix a `T.isInfixOf` fix b

    where
        fix :: Text -> Text
        fix = T.strip . T.toCaseFold
            . T.filter (`notElem` [',', '.'])

asSearchResult :: Document -> Widget
asSearchResult (Document rid r _ u) = do
    reviewTime <- lift . liftIO . humanReadableTime $ reviewCreatedDate r

    [whamlet|
        <div .row .search-result-body>
            <div .span2>
                <address>
                    #{reviewAddress r}

            <div .span10>
                <blockquote>
                    #{markdownToHtml $ shortenM 400 $ reviewContent r}

                    <small>
                        Reviewed by #{showName u} #{reviewTime} &mdash; 
                        <a href=@{ReviewsR rid}>Read more...
        |]

    where
        shortenM :: Int -> Markdown -> Markdown
        shortenM n (Markdown s) = let len = length s in
            Markdown $ if len > n then take (len - 3) s ++ "..." else s
