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
    case docByReviewId rid docs of
        Just d -> defaultLayout $ do
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

postReviewsR :: ReviewId -> Handler RepHtml
postReviewsR = getReviewsR
