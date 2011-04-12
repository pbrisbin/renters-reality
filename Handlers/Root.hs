{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Root (getRootR) where

import Yesod
import Renters
import Model
import qualified Settings

-- | Home page
getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    Settings.setTitle "Home"

    addJulius [julius|
        $(function() {
            $('.tabbed').tabs();

            $('.accordion').accordion({
                collapsible: true,
                autoHeight:  false,
                active:      false
            });

            $('.complete').autocomplete({
                source: "@{JsonLandlordsR}",
                selectFirst: true
            });
        });
        |]

    [hamlet|
        <h1>Renters' reality

        <div .tabbed>
            <ul .tabnav>
                <li #renter>
                    <a href="#renter">renters
                <li #landlord>
                    <a href="#landlord">landlords
                <li #about>
                    <a href="#about">about

            <div #renter .tabdiv>
                <div .accordion>
                    <h3>Submit a positive review
                    <div>
                        ^{landlordInput (NewR Positive) "Next"}

                    <h3>Submit a negative review
                    <div>
                        ^{landlordInput (NewR Negative) "Next"}

                    <h3>Search reviews by landlord
                    <div>
                        ^{landlordInput SearchR "Search"}

                        <div .note>
                            <p>
                                <strong>Note: 
                                Partial searches are not supported.  The 
                                autocomplete will populate with matches 
                                as you type. If you don't see a match 
                                for what you've entered, that landlord 
                                is not in the system.


                    <h3>Search reviews by property
                    <div>
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

            <div #landlord .tabdiv>
                <p>
                    Landlords,

                <p>
                    We hope to some day ofter a suite of tools to help 
                    you maintain your own reputation on this site.

                <p>
                    We would like to provide ways to track, discuss, and 
                    resolve negative reviews as well as easily use the 
                    positive reviews found here to better promote your 
                    business.

                <p>
                    If you have any ideas for how to make this site 
                    useful for yourself, please open an issue at this 
                    site's 
                    <a href="https://github.com/pbrisbin/renters-reality/issues">project page
                    \.

            <div #about .tabdiv>
                <p>
                    We aim to provide an easy to use service where 
                    residents can write a review of their landlord.

                <p>
                    Would-be residents can then search our database 
                    before unknowingly renting an apartment from a bad 
                    landlord.

                <p>
                    If you find any bugs or would like to request a 
                    feature please use the 
                    <a href="https://github.com/pbrisbin/renters-reality/issues">issues tracker
                    \.
        |]

        where

            landlordInput :: RentersRoute -> String -> Widget ()
            landlordInput route label = [hamlet|
                <form .landlord method="get" action="@{route}">
                    <p>
                        <label for="landlord">Landlord: 
                        <input .complete size=40 placeholder="Name of landlord or management company" name="landlord"> 
                        <input type="submit" value="#{label}">
                |]

            tableRow :: String -> String -> String -> Widget ()
            tableRow name label placeholder = [hamlet|
                <tr>
                    <th>
                        <label for=#{name}> #{label}
                    <td>
                        <input size=30 name=#{name} placeholder=#{placeholder}>
                |]
