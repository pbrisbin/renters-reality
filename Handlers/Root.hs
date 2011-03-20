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
    Settings.setTitle "Home"

    addJulius [$julius|
        $(function() {
            $('#maintab').tabs();

            $('.accordian').accordion({
                collapsible: true,
                autoHeight:  false,
                active:      false
            });

            $('.complete').autocomplete({
                source: "@{JsonR LandlordJ}",
                selectFirst: true
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
                        <h3>Submit a positive review
                        <div>
                            ^{landlordForm $ NewR Positive}
                        <h3>Submit a negative review
                        <div>
                            ^{landlordForm $ NewR Negative}
                        <h3>Search reviews by landlord
                        <div>
                            ^{landlordSearchForm}
                        <h3>Search reviews by property
                        <div>
                            ^{propertySearchForm}

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
                        residents can write a review of their landlord.

                    <p>
                        Would-be residents can then search our database 
                        before unknowingly renting an apartment from a 
                        bad landlord.

                    <p>
                        We also aggregate the number of positive and 
                        negative reviews, combined with the total amount 
                        of properties we know a landlord manages to come 
                        with a general rating.

                    <p>
                        We are in pre-pre-beta at this point, and not 
                        alot is working.

                    <p>
                        Assume any data present is bogus, and anything 
                        you enter may be unexpectedly removed.
        |]
