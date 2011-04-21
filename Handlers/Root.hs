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

            $('.accordion').accordion({ autoHeight: false });

            //$('.search-complete').autocomplete({
            //    source: "@{SearchR}"
            //    selectFirst: true
            //});
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
                    <h3>Search reviews
                    <div>
                        <form method="get" action="@{SearchR}">
                            <p>
                                <input .search-complete size=60 name="term" id="term"> 
                                <input type="submit" value="Search">

                    <h3>Submit a 
                        <span .positive>positive
                        \ review
                    <div>
                        ^{landlordInput (NewR Positive) "Next"}

                    <h3>Submit a 
                        <span .negative>negative
                        \ review
                    <div>
                        ^{landlordInput (NewR Negative) "Next"}

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
                        <input .landlord-complete size=40 placeholder="Name of landlord or management company" name="landlord"> 
                        <input type="submit" value="#{label}">
                |]
