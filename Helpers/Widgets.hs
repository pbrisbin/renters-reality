{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Widgets where

import Renters

import Data.Text              (Text)
import Yesod.Goodies.Markdown (markdownToHtml)
import Yesod.Goodies.Shorten  (shorten)
import Yesod.Goodies.Time     (humanReadableTime)

-- | Landlord name ... GPA
landlordGPA :: (Landlord, [Document]) -> Widget ()
landlordGPA (l, docs) = do
    let gpa' = gpa $ map (reviewGrade . review) docs
    [hamlet|
        <div .landlord-gpa>
            <p>
                #{landlordName $ l}
                <span .gpa>
                    GPA: #{show gpa'}
    |]

-- | Landlord name ... Grade
landlordGrade :: Document -> Widget ()
landlordGrade (Document _ r l _) = [hamlet|
    <div .landlord-grade>
        <p>
            <a href=@{LandlordsR $ reviewLandlord r}>#{landlordName $ l}
            <span .grade>
                Grade: #{prettyGrade $ reviewGrade r}
    |]

-- | Reviewed by when ... Grade
reviewedByGrade :: Document -> Widget ()
reviewedByGrade d@(Document _ r _ _) = reviewedBy "reviewed-by-grade" d
    [hamlet|
        <span .grade>
            Grade: #{prettyGrade $ reviewGrade r}
        |]

-- | Reviewed by when ... link to review
reviewedByLink :: Document -> Widget ()
reviewedByLink d@(Document rid _ _ _) = reviewedBy "reviewed-by-link" d
    [hamlet|
        <span .review-link>
            <a href=@{ReviewsR rid}>Read more...
        |]

reviewedBy :: Text      -- ^ div class
           -> Document  -- ^ source doc
           -> Widget () -- ^ right hand side
           -> Widget ()
reviewedBy c (Document _ r _ u) w = do
    reviewTime <- lift . humanReadableTime $ reviewCreatedDate r
    [hamlet|
        <div .#{c}>
            <p>
                Reviewed by #{showName u} #{reviewTime}
                ^{w}
            |]

reviewContentBlock :: Document
                   -> Bool -- ^ shorten?
                   -> Widget ()
reviewContentBlock (Document _ r _ _) s = do
    let short = if s then shorten 400 else id
    [hamlet|
        <div .review-address>
            <p>#{reviewAddress r}

        <div .review-content>
            <blockquote>
                #{markdownToHtml $ short $ reviewContent r}
        |]

-- | Add an auto completion via jquery
addAutoCompletion :: Text    -- ^ input id
                  -> Route m -- ^ JSON route
                  -> GWidget s m ()
addAutoCompletion ident route = addWidget $(widgetFile "_autocompletion")

-- | Add contents as a popup helpbox. NOTE: you must add some clickable 
--   element with the id "open-help" somewhere on the page
addHelpBox :: GWidget s m () -- help box contents
           -> GWidget s m ()
addHelpBox contents = addWidget $(widgetFile "_helpbox")
