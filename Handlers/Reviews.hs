{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Reviews 
    ( getReviewsR
    , postReviewsR -- comments
    ) where

import Renters
import Model
import Yesod
import Yesod.Comments
import Yesod.Comments.Markdown
import Data.List (partition)
import qualified Settings

getReviewsR :: Key Review -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case lookup' rid docs of
        Just (Document _ r l p u) -> do
            reviewTime <- humanReadableTimeDiff $ reviewCreatedDate r
            let plusMinus = getPlusMinus docs l
            defaultLayout $ do
                Settings.setTitle "View review"
                [hamlet|
                    <h1>View review
                    <div .tabdiv>
                        <h3>
                            <span .landlord>
                                <a href="@{SearchR}?term=#{landlordName l}">#{landlordName l} #{plusMinus}
                            <span .property>
                                <a href="@{SearchR}?term=#{formatProperty p}">#{formatProperty p}

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
                            ^{addCommentsAuth $ show $ rid}
                    |]

        Nothing -> notFound

lookup' :: ReviewId -> [Document] -> Maybe Document
lookup' rid docs =
    case filter ((== rid) . reviewId) docs of
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
