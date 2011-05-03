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
import Data.List (partition)
import qualified Data.Text as T
import qualified Settings

getReviewsR :: Key Review -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case lookup' rid docs of
        Just (Document _ r l p u) -> do
            reviewTime <- humanReadableTime $ reviewCreatedDate r
            let plusMinus = getPlusMinus docs l
            defaultLayout $ do
                Settings.setTitle "View review"
                [hamlet|
                    <h1>View review
                    <div .tabdiv>
                        <h3>
                            <span .landlord>
                                <a title="show other reviews for this landlord" href="@{SearchR}?term=#{landlordName l}">
                                    #{landlordName l} #{plusMinus}
                            <span .property>
                                <a title="show other reviews for this property" href="@{SearchR}?term=#{formatProperty p}">
                                    #{formatProperty p}

                        <div .view-review>
                            <p>Review:

                            <div .#{show $ reviewType r}>
                                <blockquote>
                                    #{markdownToHtml $ reviewContent r}

                        <div .review-by>
                            <p>
                                Submitted by #{showName u} #{reviewTime}

                        <h3>Discussion
                        <div .discussion>
                            ^{addCommentsAuth $ T.pack $ show $ rid}
                    |]

        Nothing -> notFound

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

getPlusMinus :: [Document] -> Landlord -> String
getPlusMinus docs l = do
    let reviews      = map review $ filter ((== l) . landlord) docs
    let (pos,neg)    = partition ((== Positive) . reviewType) reviews
    let (plus,minus) = (length pos, length neg)
    go $ plus - minus

    where
        go :: Int -> String
        go n
            | n == 0 = ""
            | n <  0 = "[ -" ++ show (abs n) ++ " ]"
            | n >  0 = "[ +" ++ show n       ++ " ]"
            | otherwise = "" -- won't happen

postReviewsR :: ReviewId -> Handler RepHtml
postReviewsR = getReviewsR
