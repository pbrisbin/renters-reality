{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Widgets where

import Foundation
import Yesod.Goodies
import Data.Text (Text)

landlordGPA :: (Landlord, [Document]) -> Widget
landlordGPA (l, docs) = do
    let gpa' = gpa $ map (reviewGrade . review) docs
    [whamlet|
        <div .row>
            <h3>
                #{landlordName $ l}
                <span .float-right>
                    GPA: #{show gpa'}
    |]

landlordGrade :: Document -> Widget
landlordGrade = undefined

reviewedByGrade :: Document -> Widget
reviewedByGrade = undefined

reviewedByLink :: Document -> Widget
reviewedByLink = undefined

reviewedBy :: Text -> Document -> Widget -> Widget
reviewedBy = undefined

reviewContentBlock :: Document
                   -> Bool -- ^ shorten?
                   -> Widget
reviewContentBlock (Document rid r _ u) s = do
    reviewTime <- lift . liftIO . humanReadableTime $ reviewCreatedDate r

    [whamlet|
        <div .row .search-result-body>
            <div .span2>
                <address>
                    #{reviewAddress r}

            <div .span10>
                <blockquote>
                    $if s
                        #{markdownToHtml $ shorten 400 $ reviewContent r}

                        <small>
                            Reviewed by #{showName u} #{reviewTime} &mdash; 
                            <a href=@{ReviewsR rid}>Read more...

                    $else
                        #{markdownToHtml $ reviewContent r}

                        <small>
                            Reviewed by #{showName u} #{reviewTime} &mdash; 
                            Grade: #{prettyGrade $ reviewGrade r}
        |]
