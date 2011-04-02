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
import Renters
import Forms
import qualified Settings

-- | Home page
getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    Settings.setTitle "Home"

    addJulius [julius|
        $(function() {
            $('#maintab').tabs();

            $('.accordian').accordion({
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
        <h1>
            <a href="@{RootR}">Renters' reality

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
                            <form .landlord method="get" action="@{NewR Positive}">
                                <p>
                                    <label for="landlord">Landlord: 
                                    <input .complete size=45 placeholder="Name of landlord or management company" name="landlord" required> 
                                    <input type="submit" value="Next">

                        <h3>Submit a negative review
                        <div>
                            <form .landlord method="get" action="@{NewR Negative}">
                                <p>
                                    <label for="landlord">Landlord: 
                                    <input .complete size=45 placeholder="Name of landlord or management company" name="landlord" required> 
                                    <input type="submit" value="Next">

                        <h3>Search reviews by landlord
                        <div>
                            <form .landlord method="get" action="@{SearchR}">
                                <p>
                                    <label for="landlord">Landlord: 
                                    <input .complete size=45 placeholder="Name of landlord or management company" name="landlord" required> 
                                    <input type="submit" value="Search">

                        <h3>Search reviews by property
                        <div>
                            ^{propertySearchForm}

            <div #landlord .tabdiv>
                <div .tabcontent>
                    <div .accordian>
                        <h3>*TODO*
                        <div>
                            <p>
                                Landlords,

                            <p>
                                We hope to some day ofter a suite of 
                                tools to help you maintain your own 
                                reputation on this site.

                            <p>
                                We would like to provide ways to track, 
                                discuss, and resolve negative reviews as 
                                well as ways to easily use the positive 
                                reviews found here to better promote 
                                your business.

                            <p>
                                If you have any ideas for how to make 
                                this site useful for yourself, please 
                                open an issue at this site's 
                                <a href="https://github.com/pbrisbin/renters-reality/issues">project page
                                \.

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
                        We are in pre-pre-beta at this point, and not 
                        a lot is working.
                    <p>
                        Assume any data present is bogus, and anything 
                        you enter may be unexpectedly removed.
        |]
