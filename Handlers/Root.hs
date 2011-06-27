{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Root (getRootR) where

import Yesod
import Yesod.Helpers.Auth
import Renters
import Model
import Data.Char (toLower)
import qualified Settings

-- | Home page
getRootR :: Handler RepHtml
getRootR = do
    muid <- maybeAuthId
    defaultLayout $ do
        Settings.setTitle "Home"

        addJulius [julius|
            $(function() {
                $('.tabbed').tabs();
                $('.accordion').accordion({ autoHeight: false });

                $('#search-input').autocomplete({
                    source:    "@{CompSearchesR}",
                    minLength: 3
                });

                $('#landlord-input').autocomplete({
                    source:    "@{CompLandlordsR}",
                    minLength: 3
                });
            });
            |]

        addCassius [cassius|
            .ui-autocomplete-loading
                background: white url(@{StaticR images_ui_anim_basic_16x16_gif}) right center no-repeat
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
                        $maybe _ <- muid
                            <div .accordion>
                                ^{searchForm}
                                ^{newForm}

                        $nothing
                            ^{searchForm}

                            <p>
                                Please 
                                <a href="@{AuthR LoginR}">log in
                                \ to submit a review.

                <div #landlord .tabdiv>
                    <p>
                        Landlords,

                    <p>
                        We hope to some day ofter a suite of tools to 
                        help you maintain your own reputation on this 
                        site.

                    <p>
                        We would like to provide ways to track, discuss, 
                        and resolve negative reviews as well as easily 
                        use the positive reviews found here to better 
                        promote your business.

                    <p>
                        If you have any ideas for how to make this site 
                        useful for yourself, please open an issue at 
                        this site's 
                        <a href="https://github.com/pbrisbin/renters-reality/issues">project page
                        \.

                <div #about .tabdiv>
                    <p>
                        We aim to provide an easy to use service where 
                        residents can write a review of their landlord.

                    <p>
                        Would-be residents can then search our database 
                        before unknowingly renting an apartment from a 
                        bad landlord.

                    <p>
                        If you find any bugs or would like to request a 
                        feature please use the 
                        <a href="https://github.com/pbrisbin/renters-reality/issues">issues tracker
                        \.
            |]

            where
                searchForm :: Widget ()
                searchForm = [hamlet|
                    <h3>Search reviews
                    <div>
                        <form .search method="get" action="@{SearchR}">
                           <p>
                               <input #search-input size=45 name="q">
                               <input type="submit" value="Search">
                    |]

                newForm :: Widget ()
                newForm = [hamlet|
                    <h3>Review your landlord
                    <div>
                        <form .new method="get" action="@{NewR}">
                            <p>
                                <input #landlord-input size=45 name="landlord" placeholder="Name of landlord or management company">
                                <input type="submit" value="Next">
                    |]
