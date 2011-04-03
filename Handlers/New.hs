{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (getNewR, postNewR) where

import Yesod
import Yesod.Markdown

import Renters
import Model
import Forms

import Control.Applicative ((<$>),(<*>))
import Data.Maybe          (fromMaybe)
import Data.Monoid         (mempty)
import Data.Time           (getCurrentTime)
import Network.Wai         (remoteHost)

import qualified Settings

data ReviewForm = ReviewForm
    { rfIp        :: String
    , rfLandlord  :: String
    , rfName      :: String
    , rfEmail     :: String
    , rfAddrOne   :: String
    , rfAddrTwo   :: Maybe String
    , rfCity      :: String
    , rfState     :: String
    , rfZip       :: String
    , rfTimeframe :: String
    , rfReview    :: Markdown
    }

getNewR :: ReviewType -> Handler RepHtml
getNewR rtype = do
    req <- getRequest
    defaultLayout $ do
        Settings.setTitle "New review"
        [hamlet|
            <h1>New review

            <div .tabdiv>
                <div .tabcontent>
                    ^{runReviewForm (getParam req "landlord") rtype}
            |]

postNewR :: ReviewType -> Handler RepHtml
postNewR = getNewR

runReviewForm :: Maybe String -> ReviewType -> Widget ()
runReviewForm ml rtype = do
    ip <- lift $ return . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- lift . runFormMonadPost $ reviewForm ml ip rtype
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess rf -> lift $ do
            tm  <- getRouteToMaster
            ref <- insertFromForm rtype rf
            redirect RedirectTemporary $ tm (ReviewsR ref)

    [hamlet|
        <form enctype="#{enctype}" method="post"> ^{form}
        |]

reviewForm :: Maybe String -- ^ maybe landlord name
           -> String      -- ^ IP address of submitter
           -> ReviewType  -- ^ positive or negative
           -> FormMonad (FormResult ReviewForm, Widget())
reviewForm ml ip rtype = do
    (fIp       , fiIp       ) <- hiddenField      (ffs ""             "ip"       ) $ Just ip
    (fLandlord , fiLandlord ) <- stringField      (ffs "Landlord:"    "landlord" ) $ ml
    (fName     , fiName     ) <- stringField      (ffs "Your name:"   "name"     ) $ Nothing
    (fEmail    , fiEmail    ) <- emailField       (ffs "Your email:"  "email"    ) $ Nothing
    (fAddrOne  , fiAddrOne  ) <- stringField      (ffs "Address (1):" "addrone"  ) $ Nothing
    (fAddrTwo  , fiAddrTwo  ) <- maybeStringField (ffs "Address (2):" "addrtwo"  ) $ Nothing
    (fCity     , fiCity     ) <- stringField      (ffs "City:"        "city"     ) $ Nothing
    (fState    , fiState    ) <- stringField      (ffs "State:"       "state"    ) $ Nothing
    (fZip      , fiZip      ) <- stringField      (ffs "Zip:"         "zip"      ) $ Nothing
    (fTimeframe, fiTimeframe) <- stringField      (ffs "Time frame:"  "timeframe") $ Nothing
    (fReview   , fiReview   ) <- markdownField    (ffs "Review:"      "review"   ) $ Nothing

    return (ReviewForm 
        <$> fIp        <*> fLandlord <*> fName
        <*> fEmail     <*> fAddrOne  <*> fAddrTwo 
        <*> fCity      <*> fState    <*> fZip 
        <*> fTimeframe <*> fReview, [hamlet|
            <table .review-form>
                ^{fieldRow fiIp}
                ^{fieldRow fiLandlord}
                ^{fieldRow fiName}
                ^{fieldRow fiEmail}
                ^{fieldRow fiAddrOne}
                ^{fieldRow fiAddrTwo}
                ^{fieldRow fiCity}
                ^{fieldRow fiState}
                ^{fieldRow fiZip}
                ^{fieldRow fiTimeframe}
                ^{fieldRow fiReview}
                <tr>
                    <td>&nbsp;
                    <td .markdown-help colspan="2">
                        <small>
                            <em>
                                Reviews are parsed as pandoc-style markdown. 
                                <a #pandoc-tips href="#">Tips.
                <tr>
                    <td>&nbsp;
                    <td .buttons colspan="2">
                        <input type="submit">
                        <input type="reset">
            |])

    where
        ffs :: String -> String -> FormFieldSettings
        ffs label id = FormFieldSettings label mempty (Just id) Nothing

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

insertFromForm :: ReviewType -> ReviewForm -> Handler Int
insertFromForm rtype rf = do
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

    reviewerId <- findOrCreate $ Reviewer
        { reviewerName      = rfName rf
        , reviewerEmail     = rfEmail rf
        , reviewerIpAddress = rfIp rf
        }

    ref <- newRef

    runDB $ insert $ Review
            { reviewReference   = ref
            , reviewType        = rtype
            , reviewCreatedDate = now
            , reviewContent     = unMarkdown $ rfReview rf
            , reviewTimeframe   = rfTimeframe rf
            , reviewReviewer    = reviewerId
            , reviewLandlord    = landlordId
            , reviewProperty    = propertyId
            }

    return ref

    where
        unMarkdown (Markdown m) = m
