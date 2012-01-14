module Handler.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    , getDeleteProfileR
    , postDeleteProfileR
    ) where

import Import
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
        addWidget $(widgetFile "profile/show")

    where
        gravatarOpts :: GravatarOptions
        gravatarOpts = defaultOptions
            { gSize    = Just $ Size 48
            , gDefault = Just MM
            }

getEditProfileR :: Handler RepHtml 
getEditProfileR = do
    (_, u)               <- requireAuth
    ((_, form), enctype) <- runFormPost $ profileForm u

    defaultLayout $ do
        setTitle "Edit profile"
        addWidget $(widgetFile "profile/edit")

postEditProfileR :: Handler RepHtml
postEditProfileR = do
    (uid, u)          <- requireAuth
    ((res, _   ), _ ) <- runFormPost $ profileForm u
    case res of
        FormSuccess ef -> saveProfile uid ef
        _              -> return ()

    getEditProfileR

getDeleteProfileR :: Handler RepHtml
getDeleteProfileR = do
    _ <- requireAuth

    defaultLayout $ do
        setTitle "Delete profile"
        addWidget $(widgetFile "profile/delete")

postDeleteProfileR :: Handler RepHtml
postDeleteProfileR = do
    (uid, _) <- requireAuth 

    runDB $ deleteWhere [ReviewReviewer ==. uid]
    runDB $ deleteWhere [IdentUser      ==. uid]
    runDB $ delete uid

    redirect $ RootR
