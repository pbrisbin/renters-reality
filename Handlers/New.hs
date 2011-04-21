{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (getNewR, postNewR) where

import Yesod
import Yesod.Markdown
import Yesod.Helpers.Auth

import Renters
import Model

import Control.Applicative ((<$>),(<*>))
import Data.Maybe          (fromMaybe)
import Data.Monoid         (mempty)
import Data.Time           (getCurrentTime)
import Network.Wai         (remoteHost)

import qualified Settings

data ReviewForm = ReviewForm
    { rfIp        :: String
    , rfLandlord  :: String
    , rfAddrOne   :: String
    , rfAddrTwo   :: Maybe String
    , rfCity      :: String
    , rfState     :: String
    , rfZip       :: String
    , rfTimeframe :: String
    , rfReview    :: Markdown
    }

data MarkdownExample = MarkdownExample
    { mdText :: String
    , mdHtml :: Widget ()
    }

getNewR :: ReviewType -> Handler RepHtml
getNewR rtype = do
    (uid, _) <- requireAuth

    req <- getRequest
    defaultLayout $ do
        Settings.setTitle "New review"

        addJulius [julius|
            $(function() {
                /* add help onclick handlers */
                $("#open-help").click(function() { $("#markdown-help").fadeIn(); return false; });
                $("#close-help").click(function() { $("#markdown-help").fadeOut(); return false; });

                /* add some placeholder texts */
                $("input#addrone").attr("placeholder"  , "248 Kelton St."          );
                $("input#addrtwo").attr("placeholder"  , "Apt 4"                   );
                $("input#timeframe").attr("placeholder", "2009 - 2010"             );
                $("textarea#review").attr("placeholder", "What was it really like?");

                /* auto complete the landlords */
                $('input#landlord').autocomplete({
                    source:     "@{CompLandlordsR}",
                    minLength : 3
                });
            });
            |]

        [hamlet|
            <h1>New review

            <div .tabdiv>
                ^{runReviewForm uid (getParam req "landlord") rtype}

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

postNewR :: ReviewType -> Handler RepHtml
postNewR = getNewR

runReviewForm :: UserId -> Maybe String -> ReviewType -> Widget ()
runReviewForm uid ml rtype = do
    ip <- lift $ return . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- lift . runFormMonadPost $ reviewForm ml ip
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess rf -> lift $ do
            tm  <- getRouteToMaster
            rid <- insertFromForm uid rtype rf
            redirect RedirectTemporary $ tm (ReviewsR rid)

    [hamlet|<form enctype="#{enctype}" method="post"> ^{form}|]

reviewForm :: Maybe String -- ^ maybe landlord name
           -> String       -- ^ IP address of submitter
           -> FormMonad (FormResult ReviewForm, Widget())
reviewForm ml ip = do
    (fIp       , fiIp       ) <- hiddenField      (ffs ""             "ip"       ) $ Just ip
    (fLandlord , fiLandlord ) <- stringField      (ffs "Landlord:"    "landlord" ) $ ml
    (fAddrOne  , fiAddrOne  ) <- stringField      (ffs "Address (1):" "addrone"  ) $ Nothing
    (fAddrTwo  , fiAddrTwo  ) <- maybeStringField (ffs "Address (2):" "addrtwo"  ) $ Nothing
    (fCity     , fiCity     ) <- stringField      (ffs "City:"        "city"     ) $ Nothing
    (fState    , fiState    ) <- stringField      (ffs "State:"       "state"    ) $ Nothing
    (fZip      , fiZip      ) <- stringField      (ffs "Zip:"         "zip"      ) $ Nothing
    (fTimeframe, fiTimeframe) <- stringField      (ffs "Time frame:"  "timeframe") $ Nothing
    (fReview   , fiReview   ) <- markdownField    (ffs "Review:"      "review"   ) $ Nothing

    return (ReviewForm 
        <$> fIp      <*> fLandlord  
        <*> fAddrOne <*> fAddrTwo <*> fCity 
        <*> fState   <*> fZip     <*> fTimeframe 
        <*> fReview, [hamlet|
            <table .review-form>
                ^{fieldRow fiIp}
                ^{fieldRow fiLandlord}

                <tr .spacer>
                    <td colspan="3">&nbsp;

                ^{fieldRow fiAddrOne}
                ^{fieldRow fiAddrTwo}
                ^{fieldRow fiCity}
                ^{fieldRow fiState}
                ^{fieldRow fiZip}
                ^{fieldRow fiTimeframe}

                <tr .spacer>
                    <td colspan="3">&nbsp;

                <tr>
                    <td>&nbsp;
                    <td colspan="2">
                        <small>
                            <em>
                                Reviews are parsed as pandoc-style markdown. 
                                <a #open-help href="#">Tips.

                ^{fieldRow fiReview}

                <tr>
                    <td .buttons colspan="2">
                        <input type="submit" value="Save">
                    <td>&nbsp;
            |])

    where
        ffs :: String -> String -> FormFieldSettings
        ffs label theId = FormFieldSettings label mempty (Just theId) Nothing

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

insertFromForm :: UserId -> ReviewType -> ReviewForm -> Handler ReviewId
insertFromForm uid rtype rf = do
    now <- liftIO getCurrentTime

    landlordId <- findOrCreate $ Landlord $ rfLandlord rf

    propertyId <- findOrCreate $ Property
        { propertyAddrOne = rfAddrOne rf
        , propertyAddrTwo = fromMaybe "" $ rfAddrTwo rf
        , propertyCity    = rfCity rf
        , propertyState   = rfState rf
        , propertyZip     = rfZip rf
        }

    _ <- findOrCreate $ Ownership propertyId landlordId

    runDB $ insert $ Review
            { reviewType        = rtype
            , reviewCreatedDate = now
            , reviewIpAddress   = rfIp rf
            , reviewContent     = unMarkdown $ rfReview rf
            , reviewTimeframe   = rfTimeframe rf
            , reviewReviewer    = uid
            , reviewLandlord    = landlordId
            , reviewProperty    = propertyId
            }

    where
        unMarkdown (Markdown m) = m
