{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Widgets where

import Renters
import Model
import Yesod
import Yesod.Goodies
import qualified Data.Text as T

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

reviewedBy :: T.Text    -- ^ div class
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
    let content = if s
        then markdownToHtml . shorten 400 $ reviewContent r
        else markdownToHtml               $ reviewContent r

    [hamlet|
        <div .review-address>
            <p>#{reviewAddress r}

        <div .review-content>
            <blockquote>
                #{content}
        |]

-- | Add the js and css required for autocompletion to work
addAutoCompletion :: T.Text       -- ^ the input's id
                  -> RentersRoute -- ^ the route providing the JSON
                  -> Widget ()
addAutoCompletion ident route = do
        addJulius [julius|
            $(function() {
                $('#{ident}').autocomplete({
                    source:    "@{route}",
                    minLength: 3
                });
            });
            |]

        addCassius [cassius|
            .ui-autocomplete-loading
                background: white url(#{staticLink "images_ui_anim_basic_16x16.gif"}) right center no-repeat

            .ui-autocomplete
              width:      60px
              background: white
              border:     solid 1px black
              border-top: none
              padding: 0px
              margin:  0px

            .ui-autocomplete li
              list-style: none
              padding:    2px 5px

            .ui-autocomplete li:hover
              color:            blue
              background-color: #dedbd1
              cursor:           pointer

            |]

-- add css js and html to provide a "tips" popup that fades in and out 
-- see "search tips" on home page for example NOTE: you must add some 
-- clickable element with the id "open-help" somewhere on the page
addHelpBox :: Widget () -- help box contents
           -> Widget ()
addHelpBox contents = do
        addJulius [julius|
            $(function() {
                /* add help onclick handlers */
                $("#open-help").click(function()  { $("#helpbox").fadeIn();  return false; });
                $("#close-help").click(function() { $("#helpbox").fadeOut(); return false; });
            });
            |]

        addCassius [cassius|
            #helpbox
                display:          none /* toggled via jquery */
                position:         fixed
                top:              25%
                left:             15%
                height:           30%
                width:            70%
                z-index:          10
                padding:          20px
                font-size:        90%
                text-align:       left
                background-color: white
                border:           solid 1px #dedbd1
                overflow-y:       auto

                -webkit-border-radius: 4px
                   -moz-border-radius: 4px
                        border-radius: 4px

                -webkit-box-shadow: 3px 3px 40px #333
                   -moz-box-shadow: 3px 3px 40px #333
                        box-shadow: 3px 3px 40px #333
            |]

        [hamlet|
            <div #helpbox>
                <span style="float: right;">
                    <a #close-help href="#">[close]

                ^{contents}
            |]
