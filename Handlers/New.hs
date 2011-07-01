{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (getNewR, postNewR) where

import Renters
import Model
import Yesod
import Helpers.Widgets
import Yesod.Goodies.Markdown
import Yesod.Helpers.Auth
import Control.Applicative ((<$>),(<*>))
import Data.Maybe          (fromMaybe)
import Data.Monoid         (mempty)
import Data.Time           (getCurrentTime)
import Network.Wai         (remoteHost)
import qualified Data.Text as T
import qualified Settings

data ReviewForm = ReviewForm
    { rfIp        :: T.Text
    , rfLandlord  :: T.Text
    , rfAddress   :: Textarea
    , rfTimeframe :: T.Text
    , rfGrade     :: Grade
    , rfReview    :: Markdown
    }

data MarkdownExample = MarkdownExample
    { mdText :: String
    , mdHtml :: Widget ()
    }

getNewR :: Handler RepHtml
getNewR = do
    (uid, _) <- requireAuth

    ml <- lookupGetParam "landlord"
    defaultLayout $ do
        Settings.setTitle "New review"

        addJulius [julius|
            $(function() {
                /* add help onclick handlers */
                $("#open-help").click(function()  { $("#markdown-help").fadeIn();  return false; });
                $("#close-help").click(function() { $("#markdown-help").fadeOut(); return false; });
            });
            |]

        addAutoCompletion "input#landlord" CompLandlordsR

        [hamlet|
            <h1>New review

            <div .tabdiv>
                ^{runReviewForm uid ml}

            <div #markdown-help>
                <span style="float: right;">
                    <a #close-help href="#">[close]

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

postNewR :: Handler RepHtml
postNewR = getNewR

runReviewForm :: UserId -> Maybe T.Text -> Widget ()
runReviewForm uid ml = do
    ip <- lift $ return . T.pack . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- lift . runFormMonadPost $ reviewForm ml ip
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess rf -> lift $ do
            tm  <- getRouteToMaster
            rid <- insertFromForm uid rf
            redirect RedirectTemporary $ tm (ReviewsR rid)

    [hamlet|<form enctype="#{enctype}" method="post">^{form}|]

reviewForm :: Maybe T.Text -- ^ maybe landlord name
           -> T.Text       -- ^ IP address of submitter
           -> FormMonad (FormResult ReviewForm, Widget())
reviewForm ml ip = do
    (fIp       , fiIp       ) <- hiddenField    (ffs ""            "ip"       ) $ Just ip
    (fLandlord , fiLandlord ) <- stringField    (ffs "Landlord:"   "landlord" ) $ ml
    (fAddress  , fiAddress  ) <- textareaField  (ffs "Address:"    "address"  ) $ Nothing
    (fTimeframe, fiTimeframe) <- stringField    (ffs "Time frame:" "timeframe") $ Nothing
    (fGrade    , fiGrade    ) <- selectField'   (ffs "Grade:"      "grade"    ) $ Nothing
    (fReview   , fiReview   ) <- markdownField  (ffs "Review:"     "review"   ) $ Nothing

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
                gradesList :: [(Grade, T.Text)]
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

        ffs :: T.Text -> T.Text -> FormFieldSettings
        ffs label theId = FormFieldSettings label mempty (Just theId) Nothing

        -- span for the input cell only
        fieldCell span fi = [hamlet|
            <th>
                <label for="#{fiIdent fi}">#{fiLabel fi}
            <td ##{fiIdent fi} colspan=#{show span}>^{fiInput fi}
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

    where
        -- | Find or create an entity, returning its key in both cases
        findOrCreate :: PersistEntity a => a -> Handler (Key a)
        findOrCreate v = return . either fst id =<< runDB (insertBy v)
