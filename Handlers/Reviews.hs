{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Reviews 
    ( getReviewsR
    , postReviewsR -- comments
    ) where

import Renters
import Model
import Yesod
import Database.Persist.Base
import Yesod.Comments
import Yesod.Goodies.Markdown
import Yesod.Goodies.Time
import qualified Data.Text as T
import qualified Settings

getReviewsR :: Key Review -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case lookup' rid docs of
        Just (Document _ r l u) -> do
            reviewTime <- humanReadableTime $ reviewCreatedDate r
            defaultLayout $ do
                Settings.setTitle "View review"
                [hamlet|
                    <h1>View review
                    <div .tabdiv>
                        <h3>
                            <div .landlord>
                                <p>#{landlordName l}

                        <div .review>
                            <p>Review:
                            <blockquote>#{markdownToHtml $ reviewContent r}

                        <div .reviewer>
                            <p>Submitted by #{showName u} #{reviewTime}

                        <h3>Discussion
                        <div .discussion>
                            ^{addCommentsAuth $ rText rid}
                    |]

        Nothing -> notFound

rText :: ReviewId -> T.Text
rText = go . unReviewId

    where
        go (PersistText  t) = t
        go (PersistInt64 i) = T.pack $ show i

-- | Somehow related to the persistent upgrade, keys are stored as 
--   PersistInt64 Int64 but when used as a singlePiece they come in as 
--   PersistText Text. This custom eq will ensure that the reviews are 
--   still found
rEq :: ReviewId -> ReviewId -> Bool
rEq a b = a == b || go (unReviewId a) (unReviewId b)

    where
        go (PersistText  t) (PersistInt64 i) = t == (T.pack $ show i)
        go (PersistInt64 i) (PersistText  t) = t == (T.pack $ show i)
        go _                _                = False

lookup' :: ReviewId -> [Document] -> Maybe Document
lookup' rid docs =
    case filter ((rEq rid) . reviewId) docs of
        []    -> Nothing
        (x:_) -> Just x

postReviewsR :: ReviewId -> Handler RepHtml
postReviewsR = getReviewsR
