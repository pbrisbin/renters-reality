{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Root (getRootR) where

import Renters
import Helpers.Widgets
import Yesod.Helpers.Auth

-- | Home page
getRootR :: Handler RepHtml
getRootR = do
    muid <- maybeAuthId
    defaultLayout $ do
        setTitle "Home"

        addJulius [julius|
            $(function() {
                $('.tabbed').tabs();
                $('.accordion').accordion({ autoHeight: false });
            });
            |]

        -- inefficient but whatever...
        addAutoCompletion "#search-input"   CompSearchesR
        addAutoCompletion "#landlord-input" CompLandlordsR

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
                    <div #main-forms>
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
                    <div #landlords-note>
                        <p>
                            Landlords,

                        <p>
                            We hope to some day ofter a suite of tools 
                            to help you maintain your own reputation on 
                            this site.

                        <p>
                            We would like to provide ways to track, 
                            discuss, and resolve negative reviews as 
                            well as easily use the positive reviews 
                            found here to better promote your business.

                        <p>
                            If you have any ideas for how to make this 
                            site useful for yourself, please open an 
                            issue at this site's 
                            <a href="https://github.com/pbrisbin/renters-reality/issues">project page
                            \.

                <div #about .tabdiv>
                    <div #about-note>
                        <p>
                            We aim to provide an easy to use service 
                            where residents can write a review of their 
                            landlord.

                        <p>
                            Would-be residents can then search our 
                            database before unknowingly renting an 
                            apartment from a bad landlord.

                        <p>
                            If you find any bugs or would like to 
                            request a feature please use the 
                            <a href="https://github.com/pbrisbin/renters-reality/issues">issues tracker
                            \.

                ^{addHelpBox helpBoxContents}
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
                        <p .search-tips>
                            <a #open-help href="#">search tips
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

helpBoxContents :: Widget ()
helpBoxContents = [hamlet|
    <h3>Search tips:

    <p>
        By default, the search will return matches on landlord name and 
        reviewed address.

    <p>
        That means that if you search for "brighton" you'll get results 
        for both "Brighton, MA" 
        <strong>and 
        "Brighton Realty"

    <p>
        Since this isn't always wanted, there are two prefixes you can 
        use to change how the search is performed:

    <table>
        <tr>
            <td>
                <code>landlord: something
            <td>
                <em>
                    Only show reviews where "something" is part of the 
                    landlord's name
        <tr>
            <td>
                <code>address: something else
            <td>
                <em>
                    Only show reviews where "something else" is part of 
                    the address

    <p>
        Results are sorted by closest match, then most recent.
    |]
