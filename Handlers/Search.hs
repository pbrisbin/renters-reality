{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search 
    ( getSearchR
    , getCompLandlordsR
    , getCompSearchesR
    ) where

import Renters
import Model
import Helpers.Widgets
import Yesod
import Yesod.Goodies
import Control.Monad (forM)
import qualified Data.Text as T
import qualified Settings

getCompLandlordsR :: Handler RepJson
getCompLandlordsR = generalCompletion $ \t -> do
    res <- runDB $ selectList [] [LandlordNameAsc] 0 0
    return . concat =<< (forM res $ \(_, (Landlord name)) ->
        return $ if t `looseMatch` name then [name] else [])

-- TODO
getCompSearchesR :: Handler RepJson
getCompSearchesR = getCompLandlordsR

-- | Get the term from the request and pass it to the completion 
--   function, serve the retured values as a list
generalCompletion :: (T.Text -> Handler [T.Text]) -> Handler RepJson
generalCompletion f = do
    mterm <- lookupGetParam "term"
    ss    <- case mterm of
        Nothing   -> return []
        Just ""   -> return []
        Just term -> f term

    jsonToRepJson . jsonList $ map (jsonScalar . T.unpack) ss

-- | A loose infix match
looseMatch :: T.Text -> T.Text -> Bool
looseMatch a b = fix a `T.isInfixOf` fix b

    where
        fix :: T.Text -> T.Text
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
        Just term -> fmap (search_ term) $ siteDocs =<< getYesod

    defaultLayout $ do
        Settings.setTitle "Search results" 

        addJulius [julius|
            $(function() {
                $('#search-input').autocomplete({
                    source:    "@{CompSearchesR}",
                    minLength: 3
                });
            });
            |]

        addCassius [cassius|
            .ui-autocomplete-loading
                background: white url(@{StaticR images_ui_anim_basic_16x16_gif}) right center no-repeat
            |]

        [hamlet|
            <div .search-form>
                <form .search method="get" action="@{SearchR}">
                   <p>
                       <input #search-input size=30 name="q">
                       <input type="submit" value="Search">

            <div .tabdiv>
                ^{paginate myPageOptions docs}
            |]

noReviews :: Widget ()
noReviews = [hamlet|
    <p>
        I'm sorry, there are no reviews that meet your search criteria.

    <p>
        Would you like to 
        <a href="@{NewR}">write one
        ?
    |]
