{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Forms
    ( AddrSearch(..)
    , propertySearchForm
    , addrFromForm
    ) where

import Yesod
import Renters

import Control.Applicative ((<$>),(<*>))

data AddrSearch = AddrSearch
    { addrOne   :: Maybe String
    , addrTwo   :: Maybe String
    , addrCity  :: Maybe String
    , addrState :: Maybe String
    , addrZip   :: String
    }

propertySearchForm :: Widget ()
propertySearchForm = [hamlet|
    <form method="post" action=@{SearchR}>
        <table>
            ^{tableRow "addrone" "Address line 1:" "248 Kelton St"}
            ^{tableRow "addrtwo" "Address line 2:" "Apt 1"        }
            ^{tableRow "city"    "City:"           ""             }
            ^{tableRow "state"   "State:"          ""             }
            <tr>
                <th>
                    <label for="zip">Zip:
                <td>
                    <input size=30 name="zip" required>

            <tr #buttons>
                <td>&nbsp;
                <td>
                    <input type=submit value="Search">
    |]
    where
        tableRow :: String -> String -> String -> Widget ()
        tableRow name label placeholder = [hamlet|
            <tr>
                <th>
                    <label for=#{name}> #{label}
                <td>
                    <input size=30 name=#{name} placeholder=#{placeholder}>
            |]

addrFromForm :: Handler AddrSearch
addrFromForm = runFormPost' $ AddrSearch
    <$> maybeStringInput "addrone"
    <*> maybeStringInput "addrtwo"
    <*> maybeStringInput "city"
    <*> maybeStringInput "state"
    <*> stringInput "zip"
