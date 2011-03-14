{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Handlers.Root
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Handlers.Root (getRootR) where

import Yesod
import BadLandlords
import qualified Settings

-- | Home page
getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    setTitle "bad boston landlords | Home"
    [$hamlet|
        <div id=maintab class=tabbed>
            <ul class=tabnav>
                <li #renter>
                    <a href="#renter">renters
                <li #landlord>
                    <a href="#landlord">landlords
                <li #about>
                    <a href="#about">about

            <div id=renter class=tabdiv>
                <div class=tabcontent>
                    <h3>Register a complaint
                    <div>
                        ^{landlordSearch NewR}
                    <h3>Search complaints by property
                    <div>
                        <p>Todo:
                    <h3>Search complaints by landlord name
                    <div>
                        ^{landlordSearch SearchR}

            <div id=landlord class=tabdiv>
                <div class=tabcontent>
                    <h3>Find complaints about you
                    <div>
                        <p>Todo
                    <h3>Declare ownership of a property
                    <div>
                        <p>Todo
                    <h3>Give up ownership of a property
                    <div>
                        <p>Todo

            <div id=about class=tabdiv>
                <div class=tabcontent>
                    <p>
                        We aim to provide an easy to use service where 
                        residents of Boston (or the surrounding area) who 
                        are dissatisfied with their landlords can register a 
                        public complaint.

                    <p>
                        Would-be residents can then search our database 
                        before unkowingly renting an apartment from a 
                        bad landlord.

                    <p>
                        The service is also designed with Landlords in mind, 
                        offering ways to search for complaints in your name, 
                        settle disputes, and declare or absolve ownership of 
                        the properties listed here.

            <script>
                $(function() {
                    $('#maintab').tabs();

                    $('.tabcontent').accordion({
                        collapsible: true,
                        autoHeight:  false,
                        active:      false
                    });

                    var landlords = [ "Rayce Realty Trust", "Brighton Realty" ];

                    $('.complete').autocomplete({
                        source: landlords
                    });
                });
        |]

-- | Posts the value as "landlord" to the route specified
landlordSearch :: BadLandlordsRoute -> Widget ()
landlordSearch route = [$hamlet|
    <form method=post action=@{route}>
        <table>
            <tr>
                <th>
                    <label for=landlord>Landlord name:
                <td>
                    <input class=complete name=landlord>
                <td id=button>
                    <input type=submit>
    |]
