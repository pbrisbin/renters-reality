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
import Forms
import qualified Settings

-- | Home page
getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    setTitle "bad boston landlords | Home"

    addJulius [$julius|
        $(function() {
            $('#maintab').tabs();

            $('.accordian').accordion({
                collapsible: true,
                autoHeight:  false,
                active:      false
            });

            // testing values
            var landlords = [ "Rayce Realty Trust"
                            , "Brighton Realty"
                            , "Joe's Realty"
                            , "Sam's Realty"
                            , "Eileen Marsky"
                            , "Joe Allan"
                            ];

            $('.complete').autocomplete({
                source: landlords
            });
        });
        |]

    [$hamlet|
        <div #maintab .tabbed>
            <ul .tabnav>
                <li #renter>
                    <a href="#renter">renters
                <li #landlord>
                    <a href="#landlord">landlords
                <li #about>
                    <a href="#about">about

            <div #renter .tabdiv>
                <div .tabcontent>
                    <div .accordian>
                        <h3>Register a complaint
                        <div>
                            ^{landlordForm NewR}
                        <h3>Search complaints by landlord name
                        <div>
                            ^{landlordForm $ SearchR LandlordS}
                        <h3>Search complaints by property
                        <div>
                            <p>Todo:

            <div #landlord .tabdiv>
                <div .tabcontent>
                    <div .accordian>
                        <h3>Find complaints about you
                        <div>
                            <p>Todo:

            <div #about .tabdiv>
                <div .tabcontent>
                    <p>
                        We aim to provide an easy to use service where 
                        residents of Boston (or the surrounding area) 
                        who are dissatisfied with their landlords can 
                        register a public complaint.

                    <p>
                        Would-be residents can then search our database 
                        before unkowingly renting an apartment from a 
                        bad landlord.

                    <p>
                        We are in pre-pre-beta at this point, and not 
                        alot is working.

                    <p>
                        Assume any data present is bogus, and anything 
                        you enter may be unexpectedly removed.
        |]
