{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes     #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  BadLandlords
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module BadLandlords where

import Yesod
import Yesod.Form.Core (GFormMonad)
import Yesod.Helpers.Static

import Control.Monad    (unless)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Text.Jasmine     (minifym)

import Database.Persist.GenericSql
import qualified Data.ByteString.Lazy as L

import qualified Settings

-- | The main site type
data BadLandlords = BadLandlords 
    { getStatic :: Static 
    , connPool   :: ConnectionPool 
    }

type Handler     = GHandler   BadLandlords BadLandlords
type Widget      = GWidget    BadLandlords BadLandlords
type FormMonad a = GFormMonad BadLandlords BadLandlords a

-- | Define all of the routes and handlers
mkYesodData "BadLandlords" [$parseRoutes|
    /       RootR    GET

    /search/#String  SearchR POST
    /complaints/#Int ComplaintsR GET

    /new/            NewR    POST
    /create          CreateR POST

    /legal  LegalR GET
    /static StaticR Static getStatic
    |]

staticFiles Settings.staticDir

instance Yesod BadLandlords where 
    approot _   = Settings.approot

    defaultLayout widget = do
        pc <- widgetToPageContent widget

        hamletToRepHtml [$hamlet|
            \<!DOCTYPE html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    <meta name="description" content="Find and report bad landlords in or around the city of boston and surrounding areas">
                    <meta name="author" content="Patrick Brisbin">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    <link rel="stylesheet" href="@{StaticR css_style_css}">
                    <script src="//ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
                    <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js">
                    ^{pageHead pc}
                <body>
                    <p #legal>
                        <a href="@{LegalR}">legal

                    <section .content>
                        <h1>Bad Boston Landlords
                        ^{pageBody pc}

                    <footer>
                        <p>
                            <small>copyright patrick brisbin 2011
            |]

    urlRenderOverride a (StaticR s) = Just $ uncurry (joinPath a Settings.staticRoot) $ renderRoute s
    urlRenderOverride _ _           = Nothing

    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                        Left _ -> content
                        Right y -> y
                    else content
        let statictmp = Settings.staticDir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

    clientSessionDuration _ = 60 * 24 * 7 -- one week

instance YesodPersist BadLandlords where
    type YesodDB BadLandlords = SqlPersist
    runDB db = liftIOHandler $ fmap connPool getYesod >>= runSqlPool db
