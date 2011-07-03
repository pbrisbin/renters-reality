{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.New (getNewR, postNewR) where

import Renters
import Model
import Yesod
import Helpers.Forms
import Helpers.Widgets
import Yesod.Goodies.Markdown
import Yesod.Helpers.Auth
import Control.Applicative ((<$>),(<*>))
import Data.Monoid         (mempty)
import Data.Time           (getCurrentTime)
import Network.Wai         (remoteHost)
import qualified Data.Text as T
import qualified Settings

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

            ^{addHelpBox helpBoxContents}
            |]

postNewR :: Handler RepHtml
postNewR = getNewR

runReviewForm :: UserId -> Maybe T.Text -> Widget ()
runReviewForm uid ml = do
    ip <- lift $ return . T.pack . show . remoteHost =<< waiRequest
    ((res, form), enctype) <- lift . runFormMonadPost $ reviewForm Nothing ml ip
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess rf -> lift $ do
            tm  <- getRouteToMaster
            rid <- insertFromForm uid rf
            redirect RedirectTemporary $ tm (ReviewsR rid)

    [hamlet|<form enctype="#{enctype}" method="post">^{form}|]
