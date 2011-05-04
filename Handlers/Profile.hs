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
import Yesod.Helpers.Auth
import Yesod.Goodies.Gravatar
import Renters
import Model

import Control.Applicative ((<$>), (<*>))
import Data.Maybe          (fromMaybe)

import qualified Data.Text as T
import qualified Settings

data EditForm = EditForm
    { eFullname :: Maybe T.Text
    , eUsername :: Maybe T.Text
    , eEmail    :: Maybe T.Text
    }

getProfileR :: Handler RepHtml
getProfileR = do
    (_, u) <- requireAuth

    let fullname = fromMaybe "" $ userFullname u
    let username = fromMaybe "" $ userUsername u
    let email    = fromMaybe "" $ userEmail u
    let pic      = gravatarImg email gravatarOpts

    defaultLayout $ do
        Settings.setTitle "View profile"
        [hamlet|
            <h1>Your profile
            <div .tabdiv>
                <div .profile>
                    <div .gravatar>
                        <a title="change your profile picute at gravatar" href="http://gravatar.com/emails/">
                            <img src="#{pic}">

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

        where
            gravatarOpts :: GravatarOptions
            gravatarOpts = defaultOptions
                { gSize    = Just $ Size 48
                , gDefault = Just MM
                }

getEditProfileR :: Handler RepHtml 
getEditProfileR = defaultLayout $ do
    Settings.setTitle "Edit profile"
    [hamlet|
        <h1>Edit profile
        <div .tabdiv>
            <div .profile>
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
    (uid, u)          <- requireAuth
    ((res, _   ), _ ) <- runFormMonadPost $ editForm u
    case res of
        FormSuccess ef -> saveChanges uid ef
        _              -> return ()

    -- we should never get here since all fields are optional
    getEditProfileR

showForm :: Widget ()
showForm = do
    (_, u)               <- lift requireAuth
    ((_, form), enctype) <- lift . runFormMonadPost $ editForm u

    [hamlet|<form enctype="#{enctype}" method="post">^{form}|]

editForm :: User -> FormMonad (FormResult EditForm, Widget())
editForm u = do
    (fFullname, fiFullname) <- maybeStringField "Full name:"     $ Just $ userFullname u
    (fUsername, fiUsername) <- maybeStringField "User name:"     $ Just $ userUsername u
    (fEmail   , fiEmail   ) <- maybeEmailField  "Email address:" $ Just $ userEmail u

    return (EditForm <$> fFullname <*> fUsername <*> fEmail, [hamlet|
            <table .edit-form>
                ^{fieldRow fiFullname}
                ^{fieldRow fiUsername}
                ^{fieldRow fiEmail}
                <tr>
                    <td .buttons colspan="2">
                        <input type="submit" value="Save">
                    <td>&nbsp;

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
        <p>There's no going back. Everything of yours will be deleted.

        <div .confirm>
            <form method="post">
                <input type="Submit" value="Yes, I'm sure.">
            <p .nothanks>
                <a href="@{EditProfileR}">No, take me back.

    |]

postDeleteProfileR :: Handler RepHtml
postDeleteProfileR = do
    (uid, _) <- requireAuth 

    -- todo: delete comments?

    -- delete reviews and the user herself
    runDB $ deleteWhere [ReviewReviewerEq uid]
    runDB $ deleteWhere [IdentUserEq uid]
    runDB $ delete uid

    redirect RedirectTemporary $ AuthR LogoutR
