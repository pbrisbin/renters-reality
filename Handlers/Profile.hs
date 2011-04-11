{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Profile
    ( getProfileR
    , getEditProfileR
    , postEditProfileR
    , getDeleteProfileR
    , postDeleteProfileR
    ) where

import Yesod
import Yesod.Form
import Yesod.Comments
import Yesod.Helpers.Auth
import Renters
import Model

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (forM)
import Data.Maybe          (fromMaybe)

import qualified Settings

data EditForm = EditForm
    { eFullname :: Maybe String
    , eUsername :: Maybe String
    , eEmail    :: Maybe String
    }

getProfileR :: Handler RepHtml
getProfileR = do
    (uid, user) <- requireAuth
    let fullname = fromMaybe "" $ userFullname user
    let username = fromMaybe "" $ userUsername user
    let email    = fromMaybe "" $ userEmail user
    defaultLayout $ do
        Settings.setTitle "View profile"
        [hamlet|
            <h1>Your profile
            <div .tabdiv>
                <div .tabcontent>
                    <table>
                        <tr>
                            <th>Full name:
                            <td>#{fullname}
                        <tr>
                            <th>User name:
                            <td>#{username}
                        <tr>
                            <th>Email address:
                            <td>#{email}

                    <p .edit-button>
                        <a href="@{EditProfileR}">edit
            |]

getEditProfileR :: Handler RepHtml 
getEditProfileR = defaultLayout $ do
    Settings.setTitle "Edit profile"
    [hamlet|
        <h1>Edit profile
        <div .tabdiv>
            <div .tabcontent>
                <p>
                    Reviews and comments will be tagged with your user 
                    name. If you leave it blank, your full name will be 
                    used in stead.

                <p>
                    Your email is not publicly displayed anywhere. It 
                    may be used in an upcoming "notifications" feature 
                    of the site and even then, only if you opt-in.

                <hr>

                ^{showForm}
        |]

postEditProfileR :: Handler RepHtml
postEditProfileR = do
    (uid, user)       <- requireAuth
    ((res, _   ), _ ) <- runFormMonadPost $ editForm uid user
    case res of
        FormSuccess ef -> saveChanges uid ef
        _              -> return ()

    -- we should never get here since all fields are optional
    getEditProfileR

showForm :: Widget ()
showForm = do
    (uid, user)          <- lift requireAuth
    ((_, form), enctype) <- lift . runFormMonadPost $ editForm uid user

    [hamlet|<form enctype="#{enctype}" method="post">^{form}|]

editForm :: UserId -> User -> FormMonad (FormResult EditForm, Widget())
editForm uid u = do
    (fFullname, fiFullname) <- maybeStringField "Full name:"     $ Just $ userFullname u
    (fUsername, fiUsername) <- maybeStringField "User name:"     $ Just $ userUsername u
    (fEmail   , fiEmail   ) <- maybeEmailField  "Email address:" $ Just $ userEmail u

    return (EditForm <$> fFullname <*> fUsername <*> fEmail, [hamlet|
            <table .edit-form>
                ^{fieldRow fiFullname}
                ^{fieldRow fiUsername}
                ^{fieldRow fiEmail}
                <tr>
                    <td>&nbsp;
                    <td .buttons colspan="2">
                        <input type="submit">
                        <input type="reset">

            <p .delete-button>
                <a href="@{DeleteProfileR}">delete
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

-- todo: unique usernames, no numer-conly usernames
saveChanges :: UserId -> EditForm -> Handler ()
saveChanges uid ef = do
    runDB $ update uid 
        [ UserFullname $ eFullname ef
        , UserUsername $ eUsername ef
        , UserEmail    $ eEmail    ef
        ]

    tm <- getRouteToMaster
    redirect RedirectTemporary $ tm ProfileR

getDeleteProfileR :: Handler RepHtml
getDeleteProfileR = defaultLayout [hamlet|
    <h1>Are you sure?
    <div .tabdiv>
        <div .tabcontent>
            <p>This feature will be implemented shortly.
    |]

postDeleteProfileR :: Handler RepHtml
postDeleteProfileR = do
    (uid, _) <- requireAuth 
    y        <- getYesod

    -- todo: delete comments?

    -- delete reviews and the user herself
    runDB $ deleteWhere [ReviewReviewerEq uid]
    runDB $ deleteWhere [IdentUserEq uid]
    runDB $ delete uid

    redirect RedirectTemporary $ AuthR LogoutR
