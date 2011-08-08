{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Root (getRootR) where

import Renters
import Helpers.Widgets
import Yesod.Helpers.Auth

getRootR :: Handler RepHtml
getRootR = do
    muid <- maybeAuthId
    defaultLayout $ do
        setTitle "Home"
        addWidget $(widgetFile "homepage")

        addAutoCompletion "#search-input"   CompSearchesR
        addAutoCompletion "#landlord-input" CompLandlordsR
        addHelpBox helpBoxContents

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
