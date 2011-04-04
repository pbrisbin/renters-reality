{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Renters
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Renters where

import Yesod
import Yesod.Markdown
import Yesod.Form.Core (GFormMonad)
import Yesod.Helpers.Static

import Data.Time
import System.Locale

import Control.Applicative ((<$>))
import Data.Char        (isSpace)
import Control.Monad    (unless)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Text.Jasmine     (minifym)

import Database.Persist.GenericSql
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import qualified Settings

-- | The main site type
data Renters = Renters
    { getStatic :: Static 
    , connPool  :: ConnectionPool 
    }

type Handler     = GHandler   Renters Renters
type Widget      = GWidget    Renters Renters
type FormMonad a = GFormMonad Renters Renters a

-- | Reviews can be good or bad
data ReviewType = Positive | Negative deriving (Show,Read,Eq)

instance SinglePiece ReviewType where
    toSinglePiece Positive = "positive"
    toSinglePiece Negative = "negative"

    fromSinglePiece "positive" = Right Positive
    fromSinglePiece "negative" = Right Negative
    fromSinglePiece _          = Left "invalid review type"

-- | Define all of the routes and handlers
mkYesodData "Renters" [parseRoutes|
    /                   RootR    GET

    /new/#ReviewType    NewR     GET POST
    /search/            SearchR  GET POST
    /reviews/#Int       ReviewsR GET POST

    /json/landlords     JsonLandlordsR  GET
    /json/reviews       JsonReviewsR    GET

    /legal              LegalR   GET
    /static             StaticR Static getStatic

    /favicon.ico FaviconR GET
    /robots.txt  RobotsR  GET
    |]

staticFiles Settings.staticDir

instance Yesod Renters where 
    approot _   = Settings.approot

    defaultLayout widget = do
        (t,h) <- breadcrumbs
        pc    <- widgetToPageContent widget
        hamletToRepHtml [hamlet|
            \<!DOCTYPE html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    <meta name="description" content="Submit and search reviews for landlords in your area.">
                    <meta name="author" content="Patrick Brisbin">
                    <meta name="viewport" content="width=device-width, initial-scale=1.0">
                    <link rel="stylesheet" href="@{StaticR css_style_css}">
                    <script src="//ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js">
                    <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js">
                    <script src="@{StaticR js_jquery_ui_autocomplete_selectFirst_js}">
                    ^{pageHead pc}
                <body>
                    <p #breadcrumbs>
                        $forall node <- h
                            <a href="@{fst node}">#{snd node} 
                            \ >
                            \ #{t}

                    <p #legal>
                        <a href="@{LegalR}">legal

                    ^{pageBody pc}

                    <footer>
                        <p>
                            <small>copyright patrick brisbin 2011. 
                            <a href="https://github.com/pbrisbin/renters-reality">source code.
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

instance YesodPersist Renters where
    type YesodDB Renters = SqlPersist
    runDB db = liftIOHandler $ fmap connPool getYesod >>= runSqlPool db

instance YesodBreadcrumbs Renters where
    breadcrumb RootR        = return ("Home"          , Nothing   )
    breadcrumb SearchR      = return ("Search reviews", Just RootR)
    breadcrumb (ReviewsR _) = return ("View review"   , Just RootR)
    breadcrumb (NewR _)     = return ("New review"    , Just RootR)
    breadcrumb LegalR       = return ("Legal info"    , Just RootR)

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)

-- | Return values by key the query string
getParam :: Request -> ParamName -> Maybe ParamValue
getParam req param = M.lookup param . M.fromList $ reqGetParams req

-- <https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs>
-- <https://github.com/snoyberg/haskellers/blob/master/LICENSE>
humanReadableTimeDiff :: UTCTime     -- ^ current time
                      -> UTCTime     -- ^ old time
                      -> String
humanReadableTimeDiff curTime oldTime =
    helper diff
  where
    diff    = diffUTCTime curTime oldTime

    minutes :: NominalDiffTime -> Double
    minutes n = realToFrac $ n / 60

    hours :: NominalDiffTime -> Double
    hours   n = minutes n / 60

    days :: NominalDiffTime -> Double
    days    n = hours n / 24

    weeks :: NominalDiffTime -> Double
    weeks   n = days n / 7

    years :: NominalDiffTime -> Double
    years   n = days n / 365

    i2s :: RealFrac a => a -> String
    i2s n = show m where m = truncate n :: Int

    old = utcToLocalTime utc oldTime

    trim = f . f where f = reverse . dropWhile isSpace

    dow           = trim $! formatTime defaultTimeLocale "%l:%M %p on %A" old
    thisYear      = trim $! formatTime defaultTimeLocale "%b %e" old
    previousYears = trim $! formatTime defaultTimeLocale "%b %e, %Y" old

    helper  d | d < 1          = "just now"
              | d < 60         = i2s d ++ " seconds ago"
              | minutes d < 2  = "one minute ago"
              | minutes d < 60 =  i2s (minutes d) ++ " minutes ago"
              | hours d < 2    = "one hour ago"
              | hours d < 24   = "about " ++ i2s (hours d) ++ " hours ago"
              | days d < 5     = "at " ++ dow
              | days d < 10    = i2s (days d)  ++ " days ago"
              | weeks d < 2    = i2s (weeks d) ++ " week ago"
              | weeks d < 5    = i2s (weeks d)  ++ " weeks ago"
              | years d < 1    = "on " ++ thisYear
              | otherwise      = "on " ++ previousYears

-- | Render from markdown, yesod-style
markdownToHtml :: Markdown -> Html
markdownToHtml = writePandoc yesodDefaultWriterOptions . parseMarkdown yesodDefaultParserStateTrusted
