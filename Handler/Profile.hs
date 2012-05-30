module Handler.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    , getDeleteProfileR
    , postDeleteProfileR
    ) where

import Import
import Helpers.Profile
import Helpers.User

import Network.Gravatar
import Data.Maybe (fromMaybe)

getProfileR :: Handler RepHtml
getProfileR = do
    (Entity uid user) <- requireAuth

    let fullname = fromMaybe "" $ userFullname user
    let username = fromMaybe "" $ userUsername user
    let email    = fromMaybe "" $ userEmail user
    let pic      = gravatar gravatarOpts email

    defaultLayout $ do
        setTitle "View profile"
        addWidget $(widgetFile "profile/show")

    where
        gravatarOpts :: GravatarOptions
        gravatarOpts = def
            { gSize    = Just $ Size 128
            , gDefault = Just MM
            }

getEditProfileR :: Handler RepHtml 
getEditProfileR = do
    (Entity _ user)      <- requireAuth
    ((_, form), enctype) <- runFormPost $ profileForm user

    defaultLayout $ do
        setTitle "Edit profile"
        addWidget $(widgetFile "profile/edit")

postEditProfileR :: Handler RepHtml
postEditProfileR = do
    (Entity uid u)    <- requireAuth
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
    (Entity uid _) <- requireAuth 

    runDB $ do
        deleteWhere [ReviewReviewer ==. uid]
        deleteWhere [IdentUser      ==. uid]
        delete uid

    redirect $ RootR
