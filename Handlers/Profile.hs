{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    , getDeleteProfileR
    , postDeleteProfileR
    ) where

import Renters
import Helpers.Forms
import Yesod.Helpers.Auth
import Yesod.Goodies.Gravatar
import Data.Maybe (fromMaybe)

getProfileR :: Handler RepHtml
getProfileR = do
    (_, u) <- requireAuth

    let fullname = fromMaybe "" $ userFullname u
    let username = fromMaybe "" $ userUsername u
    let email    = fromMaybe "" $ userEmail u
    let pic      = gravatarImg email gravatarOpts

    defaultLayout $ do
        setTitle "View profile"
        addWidget $(widgetFile "profile")

    where
        gravatarOpts :: GravatarOptions
        gravatarOpts = defaultOptions
            { gSize    = Just $ Size 48
            , gDefault = Just MM
            }

getEditProfileR :: Handler RepHtml 
getEditProfileR = defaultLayout $ do
    setTitle "Edit profile"
    runProfileFormGet

postEditProfileR :: Handler RepHtml
postEditProfileR = do
    runProfileFormPost
    getEditProfileR

getDeleteProfileR :: Handler RepHtml
getDeleteProfileR = do
    _ <- requireAuth

    defaultLayout $ do
        setTitle "Delete profile"
        addWidget $(widgetFile "deleteprofile")

postDeleteProfileR :: Handler RepHtml
postDeleteProfileR = do
    (uid, _) <- requireAuth 

    -- todo: delete comments?

    -- delete reviews and the user herself
    runDB $ deleteWhere [ReviewReviewerEq uid]
    runDB $ deleteWhere [IdentUserEq uid]
    runDB $ delete uid

    redirect RedirectTemporary $ AuthR LogoutR
