{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Reviews 
    ( getReviewsR
    , postReviewsR -- comments
    ) where

import Yesod
import Yesod.Comments
import Yesod.Markdown
import Renters
import Model

import Data.List  (intercalate, partition)
import Data.Maybe (fromMaybe)
import Data.Time  (getCurrentTime)

import qualified Settings

getReviewsR :: ReviewId -> Handler RepHtml
getReviewsR rid = do
    review    <- runDB $ get404 rid

    mlandlord <- runDB $ get $ reviewLandlord review
    mproperty <- runDB $ get $ reviewProperty review
    mreviewer <- runDB $ get $ reviewReviewer review

    case (mlandlord,mproperty,mreviewer) of
        (Just landlord, Just property, Just reviewer) -> do
            now       <- liftIO getCurrentTime
            plusminus <- getPlusMinus landlord
            defaultLayout $ do
                Settings.setTitle "View review"
                [hamlet|
                    <h1>View review
                    <div .tabdiv>
                        <h3>
                            <a href="@{SearchR}?landlord=#{landlordName landlord}">#{landlordName landlord} is a #{plusminus}
                            <span .property>#{formatProperty property}

                        <div .view-review>
                            <p>Review:

                            <div .#{show $ reviewType review}>
                                <blockquote>
                                    #{markdownToHtml $ Markdown $ reviewContent review}

                        <div .by>
                            <p>
                                Submitted by #{showName reviewer} 
                                #{humanReadableTimeDiff now $ reviewCreatedDate review}

                        <h3>Discussion
                        <div .discussion>
                            ^{addCommentsAuth $ show $ rid}
                    |]

        _ -> notFound

getPlusMinus :: Landlord -> Handler String
getPlusMinus landlord = do
    reviews <- return . map snd =<< reviewsByLandlord landlord
    let (pos,neg)    = partition ((== Positive) . reviewType) reviews
    let (plus,minus) = (length pos, length neg)
    let spread       = (-) plus minus
    return $ go spread
    where
        go n
            | n == 0 = ""
            | n <  0 = "-" ++ show (abs n)
            | n >  0 = "+" ++ show n

postReviewsR :: ReviewId -> Handler RepHtml
postReviewsR = getReviewsR

formatProperty :: Property -> String
formatProperty p = intercalate ", "
                 $ filter (not . null)
                    [ propertyAddrOne p
                    , propertyAddrTwo p
                    , propertyCity    p
                    , propertyState   p
                    ]
