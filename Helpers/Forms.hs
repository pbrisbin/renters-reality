{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Helpers.Forms
    ( reviewForm
    , insertFromForm
    , updateFromForm
    , helpBoxContents
    , runProfileFormGet
    , runProfileFormPost
    ) where

import Renters
import Yesod.Helpers.Auth
import Yesod.Goodies.Markdown
import Yesod.Form.Core     (GFormMonad)
import Control.Applicative ((<$>),(<*>))
import Data.Monoid         (mempty)
import Data.Time           (getCurrentTime)
import Data.Text           (Text)

type FormMonad a = GFormMonad Renters Renters a

data ReviewForm = ReviewForm
    { rfIp        :: Text
    , rfLandlord  :: Text
    , rfAddress   :: Textarea
    , rfTimeframe :: Text
    , rfGrade     :: Grade
    , rfReview    :: Markdown
    }

data ProfileEditForm = ProfileEditForm
    { eFullname :: Maybe Text
    , eUsername :: Maybe Text
    , eEmail    :: Maybe Text
    }


data MarkdownExample = MarkdownExample
    { mdText :: String
    , mdHtml :: Widget ()
    }

runProfileFormGet :: Widget ()
runProfileFormGet = do
    (_, u)               <- lift requireAuth
    ((_, form), enctype) <- lift . runFormMonadPost $ profileEditForm u

    [hamlet|
        <h1>Edit profile
        <div .tabdiv>
            <div .profile>
                <p>
                    Reviews and comments will be tagged with your user 
                    name. If you leave it blank, your full name will be 
                    used in stead.

                <p>
                    Your email is not publicly displayed anywhere. It is 
                    used to find your gravatar image and may be used in 
                    an upcoming "notifications" feature of the site and 
                    even then, only if you opt-in.

                <hr>

                <form enctype="#{enctype}" method="post">
                    ^{form}

                <p .delete-button>
                    <a href="@{DeleteProfileR}">delete
        |]

profileEditForm :: User -> FormMonad (FormResult ProfileEditForm, Widget())
profileEditForm u = do
    (fFullname, fiFullname) <- maybeStringField "Full name:"     $ Just $ userFullname u
    (fUsername, fiUsername) <- maybeStringField "User name:"     $ Just $ userUsername u
    (fEmail   , fiEmail   ) <- maybeEmailField  "Email address:" $ Just $ userEmail u

    return (ProfileEditForm <$> fFullname <*> fUsername <*> fEmail, [hamlet|
            <table .edit-form>
                ^{fieldRow fiFullname}
                ^{fieldRow fiUsername}
                ^{fieldRow fiEmail}
                <tr>
                    <td .buttons colspan="2">
                        <input type="submit" value="Save">
                    <td>&nbsp;
            |])

    where
        fieldRow fi = [hamlet|
            <tr ##{fiIdent fi}>
                <th>
                    <label for="#{fiIdent fi}">#{fiLabel fi}
                    <div .tooltip>#{fiTooltip fi}
                <td>
                    ^{fiInput fi}
                <td>
                    $maybe error <- fiErrors fi
                        #{error}
                    $nothing
                        &nbsp;
            |]

runProfileFormPost :: Handler ()
runProfileFormPost = do
    (uid, u)          <- requireAuth
    ((res, _   ), _ ) <- runFormMonadPost $ profileEditForm u
    case res of
        FormSuccess ef -> saveChanges uid ef
        _              -> return ()

    where
        saveChanges :: UserId -> ProfileEditForm -> Handler ()
        saveChanges uid ef = do
            runDB $ update uid 
                [ UserFullname $ eFullname ef
                , UserUsername $ eUsername ef
                , UserEmail    $ eEmail    ef
                ]

            tm <- getRouteToMaster
            redirect RedirectTemporary $ tm ProfileR

reviewForm :: Maybe Review -- ^ for use in edit
           -> Maybe Text -- ^ maybe landlord name (for use in new)
           -> Text       -- ^ IP address of submitter
           -> FormMonad (FormResult ReviewForm, Widget())
reviewForm mr ml ip = do
    (fIp       , fiIp       ) <- hiddenField    (ffs ""            "ip"       ) $ Just ip
    (fLandlord , fiLandlord ) <- stringField    (ffs "Landlord:"   "landlord" ) $ ml
    (fAddress  , fiAddress  ) <- textareaField  (ffs "Address:"    "address"  ) $ fmap reviewAddress   mr
    (fTimeframe, fiTimeframe) <- stringField    (ffs "Time frame:" "timeframe") $ fmap reviewTimeframe mr
    (fGrade    , fiGrade    ) <- selectField'   (ffs "Grade:"      "grade"    ) $ fmap reviewGrade     mr
    (fReview   , fiReview   ) <- markdownField  (ffs "Review:"     "review"   ) $ fmap reviewContent   mr

    return (ReviewForm 
        <$> fIp      <*> fLandlord  
        <*> fAddress <*> fTimeframe  
        <*> fGrade <*> fReview, [hamlet|
            <table .review-form>
                <tr #ip>^{fieldCell 4 fiIp}

                <tr #landlord-grade>
                    ^{fieldCell 1 fiLandlord}
                    ^{fieldCell 1 fiGrade}

                <tr #timeframe>
                    ^{fieldCell 4 fiTimeframe}
                    <td colspan=3>&nbsp;

                <tr #address>
                    ^{fieldCell 4 fiAddress}
                    <td colspan=3>&nbsp;

                <tr #review-help>
                    <td>&nbsp;
                    <td colspan="5">
                        <small>
                            <em>
                                Reviews are parsed as pandoc-style markdown. 
                                <a #open-help href="#">Tips.

                <tr #review>^{fieldCell 4 fiReview}

                <tr>
                    <td>&nbsp;
                    <td .buttons colspan="4">
                        <input type="submit" value="Save">
            |])

        where
            selectField' = selectField gradesList
                where
                    gradesList :: [(Grade, Text)]
                    gradesList = [ (Aplus , "A+")
                                 , (A     , "A" )
                                 , (Aminus, "A-")
                                 , (Bplus , "B+")
                                 , (B     , "B" )
                                 , (Bminus, "B-")
                                 , (Cplus , "C+")
                                 , (C     , "C" )
                                 , (Cminus, "C-")
                                 , (Dplus , "D+")
                                 , (D     , "D" )
                                 , (Dminus, "D-")
                                 , (F     , "F" )
                                 ]

            ffs :: Text -> Text -> FormFieldSettings
            ffs label theId = FormFieldSettings label mempty (Just theId) Nothing

            -- span for the input cell only
            fieldCell :: Int -> FieldInfo s m -> GWidget s m ()
            fieldCell colspan fi = [hamlet|
                <th>
                    <label for="#{fiIdent fi}">#{fiLabel fi}
                <td ##{fiIdent fi} colspan=#{show colspan}>^{fiInput fi}
                <td>
                    $maybe error <- fiErrors fi
                        #{error}
                    $nothing
                        &nbsp;
                |]

insertFromForm :: UserId -> ReviewForm -> Handler ReviewId
insertFromForm uid rf = do
    now        <- liftIO getCurrentTime
    landlordId <- findOrCreate $ Landlord $ rfLandlord rf

    runDB $ insert $ Review
            { reviewCreatedDate = now
            , reviewIpAddress   = rfIp rf
            , reviewGrade       = rfGrade rf
            , reviewAddress     = rfAddress rf
            , reviewContent     = rfReview rf
            , reviewTimeframe   = rfTimeframe rf
            , reviewReviewer    = uid
            , reviewLandlord    = landlordId
            }

updateFromForm :: ReviewId -> UserId -> ReviewForm -> Handler ReviewId
updateFromForm rid _ rf = do
    -- might've changed
    landlordId <- findOrCreate $ Landlord $ rfLandlord rf

    runDB $ update rid [ ReviewLandlord landlordId
                       , ReviewGrade     $ rfGrade    rf
                       , ReviewAddress   $ rfAddress  rf
                       , ReviewTimeframe $ rfTimeframe rf
                       , ReviewContent   $ rfReview   rf
                       ]

    -- for type consistency
    return rid

-- | Find or create an entity, returning its key in both cases
findOrCreate :: PersistEntity a => a -> Handler (Key a)
findOrCreate v = return . either fst id =<< runDB (insertBy v)

-- the markdown help tips
helpBoxContents :: Widget ()
helpBoxContents = [hamlet|
        <h3>Some quick examples:

        $forall mdExample <- mdExamples
            <p .example>
                <code>#{mdText mdExample} 
                will render as ^{mdHtml mdExample}

        <p>
            <em>
                Additional documentation can be found 
                <a href="http://daringfireball.net/projects/markdown/syntax">here
                \.
    |]

mdExamples :: [MarkdownExample]
mdExamples = [ MarkdownExample "*italic text*"
                    [hamlet|<em>italic text|]

             , MarkdownExample "**bold text**"
                    [hamlet|<strong>bold text|]

             , MarkdownExample "[some link](http://example.com \"link title\")"
                    [hamlet|<a href="http://example.com" title="link title">some link|]

             , MarkdownExample "![even images](http://pbrisbin.com/static/images/feed.png)"
                    [hamlet|<img alt="even images" src="http://pbrisbin.com/static/images/feed.png">|]
             ]
