{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Search 
    ( getSearchR
    , getCompLandlordsR
    , getCompSearchesR
    ) where

import Renters
import Model
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

getCompSearchesR :: Handler RepJson
getCompSearchesR = generalCompletion $ \t -> do
    resL <- runDB $ selectList [] [LandlordNameAsc] 0 0
    resP <- runDB $ selectList [] [PropertyZipAsc]  0 0

    -- autocomplete landlord names
    landSS <- forM resL $ \(_, (Landlord name)) ->
        return $ if t `looseMatch` name then [name] else []

    -- autocomplete property strings
    propSS <- forM resP $ \(_, p) ->
        return $ if t `looseMatch` formatProperty p then [formatProperty p] else []

    return $ concat $ landSS ++ propSS

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
        fix = T.strip
            . T.toCaseFold
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
                    ^{shortReview doc}
            |]
    }

getSearchR :: Handler RepHtml
getSearchR = do
    mterm <- lookupGetParam "term"
    case mterm of
        Nothing   -> allReviews
        Just ""   -> allReviews
        Just term -> do
            docs <- fmap (search_ term) $ siteDocs =<< getYesod
            defaultLayout $ do
                Settings.setTitle "Search results" 
                [hamlet|
                    <h1>Search results
                    <div .tabdiv>
                        ^{paginate myPageOptions docs}
                    |]

allReviews :: Handler RepHtml
allReviews = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        Settings.setTitle "All reviews"
        [hamlet|
            <h1>All reviews
            <div .tabdiv>
                ^{paginate myPageOptions docs}
            |]

noReviews :: Widget ()
noReviews = [hamlet|
    <p>
        I'm sorry, there are no reviews that meet your search criteria.

    <p>
        Would you like to 
        <a title="submit a positive review" href="@{NewR Positive}">write 
        <a title="submit a negative review" href="@{NewR Negative}">one
        ?
    |]

shortReview :: Document -> Widget ()
shortReview (Document rid r l p u) = do
    let content = markdownToHtml . shorten 400 $ reviewContent r
    reviewTime <- lift . humanReadableTime $ reviewCreatedDate r
    
    [hamlet|
        <div .review>
            <div .#{show $ reviewType r}>
                <div .property>
                    <p>#{landlordName l} - #{formatProperty p}
                <div .content>
                    #{content}
                <div .by>
                    <p>
                        Reviewed by #{showName u} #{reviewTime} 
                        <span .view-link>
                            <a href="@{ReviewsR $ rid}">View
        |]
