{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Edit (getEditR, postEditR) where

import Renters
import Model
import Yesod
import Helpers.Forms
import Helpers.Widgets
import Yesod.Helpers.Auth
import Network.Wai         (remoteHost)
import qualified Data.Text as T
import qualified Settings

getEditR :: ReviewId -> Handler RepHtml
getEditR rid = do
    (uid, _) <- requireAuth
    docs <- siteDocs =<< getYesod
    case docByReviewId rid docs of
        Just d -> defaultLayout $ do
            Settings.setTitle "Edit review"

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
                    ^{runReviewForm d uid}

                ^{addHelpBox helpBoxContents}
                |]

        _ -> notFound

postEditR :: ReviewId -> Handler RepHtml
postEditR = getEditR

runReviewForm :: Document -> UserId -> Widget ()
runReviewForm (Document rid r l _) uid = do
    ip <- lift $ return . T.pack . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- lift . runFormMonadPost $ reviewForm (Just r) (Just $ landlordName l) ip
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess rf -> lift $ do
            tm  <- getRouteToMaster
            _   <- updateFromForm rid uid rf
            redirect RedirectTemporary $ tm (ReviewsR rid)

    [hamlet|<form enctype="#{enctype}" method="post">^{form}|]
