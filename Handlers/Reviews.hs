{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Reviews 
    ( getReviewsR
    , postReviewsR -- comments
    ) where

import Renters
import Model
import Yesod
import Helpers.Widgets
import Database.Persist.Base
import Yesod.Comments
import qualified Data.Text as T
import qualified Settings

getReviewsR :: Key Review -> Handler RepHtml
getReviewsR rid = do
    docs <- siteDocs =<< getYesod
    case lookup' rid docs of
        Just d -> do
            defaultLayout $ do
                Settings.setTitle "View review"
                [hamlet|
                    <div .tabdiv>
                        <div .view-review>
                            ^{landlordGrade d}
                            ^{reviewContentBlock d False}
                            ^{reviewedBy "reviewed-by" d nothing}

                        <h3>Discussion
                        <div .discussion>
                            ^{addCommentsAuth $ rText rid}
                    |]

        Nothing -> notFound

    where
        -- reviewed by with nothing on the right
        nothing :: Widget ()
        nothing = return ()

rText :: ReviewId -> T.Text
rText = go . unReviewId

    where
        go (PersistText  t) = t
        go (PersistInt64 i) = T.pack $ show i
        go _                = ""

lookup' :: ReviewId -> [Document] -> Maybe Document
lookup' rid docs =
    case filter ((rEq rid) . reviewId) docs of
        []    -> Nothing
        (x:_) -> Just x

    where

        rEq :: ReviewId -> ReviewId -> Bool
        rEq a b = a == b || go (unReviewId a) (unReviewId b)

        go (PersistText  t) (PersistInt64 i) = t == (T.pack $ show i)
        go (PersistInt64 i) (PersistText  t) = t == (T.pack $ show i)
        go _                _                = False

postReviewsR :: ReviewId -> Handler RepHtml
postReviewsR = getReviewsR
