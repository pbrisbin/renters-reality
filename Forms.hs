{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Forms
    ( landlordForm
    , landlordFromForm
    , addressFormFields
    , addressFromForm
    , userInfoFields
    ) where

import Yesod
import BadLandlords
import Model

import Control.Applicative ((<$>),(<*>))

-- | The landlord entry is a one field input, so the button is included 
--   here. Pass the route th POST to.
landlordForm :: BadLandlordsRoute -> Widget ()
landlordForm route = [$hamlet|
    <form .landlord method="post" action="@{route}">
        <table>
            <tr>
                <th>
                    <label for="landlord">Landlord name:
                <td>
                    <input .complete size=45 placeholder="Name of landlord or management company" name="landlord" required>
                <td .landlord-button>
                    <input type="submit" value="Next">
    |]

landlordFromForm :: Handler Landlord
landlordFromForm = do
    landlord <- runFormPost' $ stringInput "landlord"
    return $ Landlord landlord

-- | Just the fields, as these will be part of a larger form some of the 
--   time
addressFormFields :: Widget ()
addressFormFields = do
    -- these are optional, zip is required
    let rows = [ ("address_one", "Address line 1:", "123 Main St")
               , ("address_two", "Address line 2:", "Apt 2"      )
               , ("city"       , "City:"          , ""           )
               , ("state"      , "State:"         , ""           )
               ]

    [$hamlet|
        $forall row <- rows
            ^{addTableRow row}

        <tr>
            <th>
                <label for="zip">Zip:
            <td>
                <input size=30 name="zip" required>
        |]
    where
        addTableRow :: (String, String, String) -> Widget ()
        addTableRow (name, label, placeholder) = [$hamlet|
            <tr>
                <th>
                    <label for=#{name}>#{label}
                <td>
                    <input size=30 name=#{name} placeholder=#{placeholder}>
            |]

addressFromForm :: Handler Addr
addressFromForm = do
    addr <- runFormPost' $ Addr
        <$> maybeStringInput "address_one"
        <*> maybeStringInput "address_two"
        <*> maybeStringInput "city"
        <*> maybeStringInput "state"
        <*> stringInput "zip"

    return addr

userInfoFields :: Widget ()
userInfoFields = [$hamlet|
    <tr>
        <th>
            <label for="author-name">Name:
        <td>
            <input size=30 name="author-name" required>
    <tr>
        <th>
            <label for="author-email">Email:
        <td>
            <input type="email" size=30 name="autho-email" required>
    |]
