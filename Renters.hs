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
import Yesod.Comments
import Yesod.Comments.Storage
import Yesod.Markdown
import Yesod.Form.Core (GFormMonad)
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Auth.Facebook
import Yesod.Helpers.Static

import Data.Time
import System.Locale

import Control.Applicative ((<$>))
import Control.Monad       (unless, liftM)
import Data.Char           (isSpace)
import Data.List           (intercalate)
import Data.Maybe          (fromMaybe, fromJust)
import System.Directory    (doesFileExist, createDirectoryIfMissing)
import Text.Jasmine        (minifym)
import Web.Routes          (encodePathInfo)

import Database.Persist.GenericSql
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import Model

import qualified Settings

-- | The main site type
data Renters = Renters
    { getStatic :: Static 
    , connPool  :: ConnectionPool 
    }

type Handler     = GHandler   Renters Renters
type Widget      = GWidget    Renters Renters
type FormMonad a = GFormMonad Renters Renters a

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
    /reviews/#ReviewId  ReviewsR GET POST

    /profile         ProfileR        GET
    /profile/edit    EditProfileR    GET POST
    /profile/delete  DeleteProfileR  GET

    /json/landlords     JsonLandlordsR  GET
    /json/reviews       JsonReviewsR    GET

    /legal              LegalR   GET
    /static             StaticR Static getStatic
    /auth               AuthR   Auth   getAuth

    /favicon.ico FaviconR GET
    /robots.txt  RobotsR  GET
    |]

staticFiles Settings.staticDir

instance Yesod Renters where 
    approot   _ = Settings.approot
    authRoute _ = Just $ AuthR LoginR

    -- fix for OpenId
    joinPath _ ar pieces qs = ar ++ '/' : encodePathInfo pieces qs
        where
            pieces'
                | pieces == ["page", "openid", "complete"] = ["page", "openid", "complete", ""]
                | otherwise = pieces

    cleanPath _ ["page", "openid", "complete", ""] = Right ["page", "openid", "complete"]
    cleanPath _ s = if corrected == s then Right s else Left corrected
        where
            corrected = filter (not . null) s

    defaultLayout widget = do
        (t,h) <- breadcrumbs
        mmesg <- getMessage
        pc    <- widgetToPageContent widget

        mauth   <- maybeAuth
        authNav <- return . pageBody =<< widgetToPageContent (authNavHelper mauth)

        hamletToRepHtml [hamlet|
            \<!DOCTYPE html>
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <link href="http://fonts.googleapis.com/css?family=Cardo" rel=stylesheet type=text/css>
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
                    <div #breadcrumbs>
                        <p>
                            $forall node <- h
                                <a href="@{fst node}">#{snd node} 
                                \ > 
                            #{t}

                    <div #right-nav>
                        <p>
                            ^{authNav}
                            \ | 
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
    breadcrumb RootR          = return ("Home"   , Nothing      )
    breadcrumb SearchR        = return ("search" , Just RootR   )
    breadcrumb (ReviewsR _)   = return ("view"   , Just RootR   )
    breadcrumb (NewR _)       = return ("new"    , Just RootR   )
    breadcrumb LegalR         = return ("legal"  , Just RootR   )
    breadcrumb (AuthR _)      = return ("login"  , Just RootR   )
    breadcrumb ProfileR       = return ("profile", Just RootR   )
    breadcrumb EditProfileR   = return ("edit"   , Just ProfileR)
    breadcrumb DeleteProfileR = return ("delete" , Just ProfileR)

instance YesodComments Renters where
    getComment    = getCommentPersist
    storeComment  = storeCommentPersist
    deleteComment = deleteCommentPersist
    loadComments  = loadCommentsPersist

    displayUser uid = do
        muser <- runDB $ get uid
        case muser of
            Just user -> return $ showName user
            Nothing   -> return ""

instance YesodAuth Renters where
    type AuthId Renters = UserId

    loginDest _  = RootR
    logoutDest _ = RootR

    getAuthId creds = do
        muid <- maybeAuth
        x <- runDB $ getBy $ UniqueIdent $ credsIdent creds
        case (x, muid) of
            (Just (_, i), Nothing) -> return $ Just $ identUser i
            (Nothing, Nothing) -> runDB $ do
                uid <- insert $ User
                    { userFullname      = Nothing
                    , userUsername      = Nothing
                    , userEmail         = Nothing
                    , userVerifiedEmail = False
                    , userVerkey        = Nothing
                    }
                _ <- insert $ Ident (credsIdent creds) uid
                return $ Just uid
            (Nothing, Just (uid, _)) -> do
                setMessage "Identifier added to your account"
                _ <- runDB $ insert $ Ident (credsIdent creds) uid
                return $ Just uid
            (Just _, Just _) -> do -- todo: what is this use case?
                setMessage "That identifier is already attached to an account. Please detach it from the other account first."
                redirect RedirectTemporary ProfileR

    showAuthId _ = showIntegral
    readAuthId _ = readIntegral

    authPlugins = [ authOpenId ]

    loginHandler = defaultLayout [hamlet|
        <div #login>
            <h3>Please login using one of the following:
            <div .services>
                <table>
                    <tr>
                        <td>
                            <form method="get" action="@{AuthR forwardUrl}" .button .google>
                                <input type="hidden" name="openid_identifier" value="https://www.google.com/accounts/o8/id">
                                <input type="image" src="@{StaticR google_gif}" value="Login via Google">
                        <td>
                            <form method="get" action="@{AuthR forwardUrl}" .button .yahoo>
                                <input type="hidden" name="openid_identifier" value="http://me.yahoo.com">
                                <input type="image" src="@{StaticR yahoo_gif}" value="Login via Yahoo!">

            <div .open-id>
                <h3>&mdash; OR &mdash;
                <table>
                    <tr>
                        <td>
                            <form method="get" action="@{AuthR forwardUrl}">
                                <input id="openid_identifier" type="text" name="openid_identifier" value="http://">
                                <input id="openid_submit" type="submit" value="Login via OpenID">
        |]

-- | Favicon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- | Robots
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)

-- | Return values by key from the query string
getParam :: Request -> ParamName -> Maybe ParamValue
getParam req param = M.lookup param . M.fromList $ reqGetParams req

authNavHelper :: Maybe (UserId, User) -> GWidget s Renters ()
authNavHelper Nothing         = [hamlet|<a href="@{AuthR LoginR}">login|]
authNavHelper (Just (uid, u)) = [hamlet|
    <a href="@{ProfileR}" title="Manage your profile">#{showName u}
    \ | 
    <a href="@{AuthR LogoutR}">logout
    |]

-- General db helpers:

-- | Find or create an entity, returning its key in both cases
findOrCreate :: PersistEntity a => a -> Handler (Key a)
findOrCreate v = do
    result <- runDB $ insertBy v
    case result of
        Left (k,v') -> return k
        Right k     -> return k

-- | Find an entity by its key
findByKey :: PersistEntity a => Key a -> Handler (Maybe a)
findByKey key = runDB $ get key

-- | Takes a filter type constructor (SqlFooEq) and a Maybe value, if 
--   the value is not Nothing or Just "", then it returns a listed 
--   application of the constructor on the unwrapped value ([SqlFooEq 
--   x]) to be added to a select statement, otherwise the list is 
--   returned empty and that condition is discarded by the caller. It 
--   sounds more complicated than it really is...
--
--   todo: generalize this beyond Maybe String...
--
maybeCriteria :: (String -> t) -> Maybe String -> [t]
maybeCriteria f v = if notNull v then [ f (fromJust v) ] else []
    where
        notNull :: Maybe String -> Bool
        notNull Nothing   = False
        notNull (Just "") = False
        notNull _         = True

-- Site-specific db helpers

reviewsByLandlord :: Landlord -> Handler [(ReviewId, Review)]
reviewsByLandlord landlord = do
    key <- findOrCreate landlord
    runDB $ selectList [ReviewLandlordEq key] [ReviewCreatedDateDesc] 0 0

reviewsByProperty :: [Property] -> Handler [(ReviewId, Review)]
reviewsByProperty properties = liftM concat $ mapM go properties 
    where
        go :: Property -> Handler [(ReviewId, Review)]
        go property = do
            key <- findOrCreate property
            runDB $ selectList [ReviewPropertyEq key] [ReviewCreatedDateDesc] 0 0

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
markdownToHtml = writePandoc yesodDefaultWriterOptions . parseMarkdown yesodDefaultParserState
