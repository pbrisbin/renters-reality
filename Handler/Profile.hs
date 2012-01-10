{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    , getDeleteProfileR
    , postDeleteProfileR
    ) where

import Foundation
import Helpers.Forms
import Yesod.Goodies
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

    runDB $ deleteWhere [ReviewReviewer ==. uid]
    runDB $ deleteWhere [IdentUser      ==. uid]
    runDB $ delete uid

    redirect RedirectTemporary $ AuthR LogoutR