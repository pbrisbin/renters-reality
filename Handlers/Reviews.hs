{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Reviews 
    ( getReviewsR
    , postReviewsR -- comments
    ) where

import Yesod
import Renters
import Model

import Data.List  (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time  (getCurrentTime)

import qualified Settings

getReviewsR :: Int -> Handler RepHtml
getReviewsR ref = do
    mrev <- runDB $ getBy (UniqueReview ref)
    case mrev of
        Nothing          -> notFound
        Just (k, review) -> do
            mlandlord <- findByKey (reviewLandlord  review)
            mproperty <- findByKey (reviewProperty  review)
            mreviewer <- findByKey (reviewReviewer  review)
            case (mlandlord,mproperty,mreviewer) of
                (Just landlord, Just property, Just reviewer) -> do
                    now <- liftIO getCurrentTime
                    defaultLayout $ do
                        Settings.setTitle $ "Review #" ++ show ref
                        [hamlet|
                            <h2>Review ##{show ref}
                            <div .tabdiv>
                                <div .tabcontent>
                                    <h3>
                                        #{landlordName landlord}
                                        \ - #{formatProperty property}

                                    <p>
                                        Submitted #{humanReadableTimeDiff now $ reviewCreatedDate review} 
                                        by #{reviewerName reviewer}

                                    <p>
                                        <strong>review:

                                    <blockquote>
                                        #{Textarea $ reviewContent review}
                            |]

                _ -> notFound

postReviewsR :: Int -> Handler RepHtml
postReviewsR = getReviewsR

formatProperty :: Property -> String
formatProperty p = intercalate ", "
                 $ filter (not . null)
                    [ propertyAddrOne p
                    , propertyAddrTwo p
                    , propertyCity    p
                    , propertyState   p
                    , propertyZip     p
                    ]
