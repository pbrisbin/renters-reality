{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Widgets where

import Foundation
import Yesod.Goodies
import Data.Text (Text)

-- | Landlord name ... GPA
landlordGPA :: (Landlord, [Document]) -> Widget
landlordGPA (l, docs) = do
    let gpa' = gpa $ map (reviewGrade . review) docs
    [whamlet|
        <div .landlord-gpa>
            <p>
                #{landlordName $ l}
                <span .gpa>
                    GPA: #{show gpa'}
    |]

-- | Landlord name ... Grade
landlordGrade :: Document -> Widget
landlordGrade (Document _ r l _) = [whamlet|
    <div .landlord-grade>
        <p>
            <a href=@{LandlordsR $ reviewLandlord r}>#{landlordName $ l}
            <span .grade>
                Grade: #{prettyGrade $ reviewGrade r}
    |]

-- | Reviewed by when ... Grade
reviewedByGrade :: Document -> Widget
reviewedByGrade d@(Document _ r _ _) = reviewedBy "reviewed-by-grade" d
    [whamlet|
        <span .grade>
            Grade: #{prettyGrade $ reviewGrade r}
        |]

-- | Reviewed by when ... link to review
reviewedByLink :: Document -> Widget
reviewedByLink d@(Document rid _ _ _) = reviewedBy "reviewed-by-link" d
    [whamlet|
        <span .review-link>
            <a href=@{ReviewsR rid}>Read more...
        |]

reviewedBy :: Text      -- ^ div class
           -> Document  -- ^ source doc
           -> Widget    -- ^ right hand side
           -> Widget
reviewedBy c (Document _ r _ u) w = do
    reviewTime <- lift . liftIO . humanReadableTime $ reviewCreatedDate r
    [whamlet|
        <div .#{c}>
            <p>
                Reviewed by #{showName u} #{reviewTime}
                ^{w}
            |]

reviewContentBlock :: Document
                   -> Bool -- ^ shorten?
                   -> Widget
reviewContentBlock (Document _ r _ _) s = do
    let short = if s then shorten 400 else id
    [whamlet|
        <div .review-address>
            <p>#{reviewAddress r}

        <div .review-content>
            <blockquote>
                #{markdownToHtml $ short $ reviewContent r}
        |]

-- | Add contents as a popup helpbox. NOTE: you must add some clickable 
--   element with the id "open-help" somewhere on the page
addHelpBox :: Widget -- ^ help box contents
           -> Widget
addHelpBox contents = addWidget $(widgetFile "_helpbox")
