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

import qualified Data.Map as M
import qualified Settings

getReviewsR :: ReviewId -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case M.lookup rid docs of
        Just (Document landlord property review user) -> do
            reviewTime <- humanReadableTimeDiff $ reviewCreatedDate review
            let plusMinus = getPlusMinus (map snd $ M.toList docs) landlord
            defaultLayout $ do
                Settings.setTitle "View review"
                [hamlet|
                    <h1>View review
                    <div .tabdiv>
                        <h3>
                            <span .landlord>
                                <a href="@{SearchR}?landlord=#{landlordName landlord}">#{landlordName landlord} #{plusMinus}
                            <span .property>
                                <a href="@{SearchR}?property=#{formatProperty property}">#{formatProperty property}

                        <div .view-review>
                            <p>Review:

                            <div .#{show $ reviewType review}>
                                <blockquote>
                                    #{markdownToHtml $ Markdown $ reviewContent review}

                        <div .review-by>
                            <p>
                                Submitted by #{showName user} #{reviewTime}

                        <h3>Discussion
                        <div .discussion>
                            ^{addCommentsAuth $ show $ rid}
                    |]

        _ -> notFound

getPlusMinus :: [Document] -> Landlord -> String
getPlusMinus docs l = do
    let reviews = map review $ filter ((== l) . landlord) docs
    let (pos,neg)    = partition ((== Positive) . reviewType) reviews
    let (plus,minus) = (length pos, length neg)
    go $ plus - minus
    where
        go n
            | n == 0 = ""
            | n <  0 = "[ -" ++ show (abs n) ++ " ]"
            | n >  0 = "[ +" ++ show n       ++ " ]"
            | otherwise = "" -- won't happen

postReviewsR :: ReviewId -> Handler RepHtml
postReviewsR = getReviewsR
