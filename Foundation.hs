module Foundation
    ( Renters (..)
    , Route (..)
    , RentersMessage (..)
    , resourcesRenters
    , Handler
    , Widget
    , Form
    , maybeAuth
    , maybeAuthId
    , requireAuth
    , requireAuthId
    , module Yesod
    , module Settings
    , module Model
    ) where

import Prelude
import Yesod hiding (setTitle)
import Yesod.Static
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Auth.Facebook
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
#ifdef DEVELOPMENT
import Yesod.Logger (logLazyText)
#endif
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (setTitle, widgetFile)
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
#else
import Network.Mail.Mime (sendmail)
#endif

import Helpers.ErrorHandler

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Renters = Renters
    { settings :: AppConfig DefaultEnv ()
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , siteDocs    :: GHandler Renters Renters [Document]
    }

-- Set up i18n messages. See the message folder.
mkMessage "Renters" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype RentersRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Renters = RentersRoute
-- * Creates the value resourcesRenters which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Renters. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the RentersRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Renters" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Renters Renters (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Renters where
    approot = appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        y <- getYesod
        mmsg <- getMessage
        mauth <- maybeAuth

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Enable Javascript async loading
    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

    errorHandler = rentersErrorHandler

-- How to run database actions.
instance YesodPersist Renters where
    type YesodPersistBackend Renters = SqlPersist
    runDB f = fmap connPool getYesod >>= Database.Persist.Store.runPool (undefined :: Settings.PersistConfig) f

instance YesodAuth Renters where
    type AuthId Renters = UserId

    -- Where to send a user after successful login
    loginDest _ = ProfileR

    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (Entity _ e) -> return . Just . identUser $ e
            Nothing -> do
                uid <- insert $ User
                    { userFullname      = Nothing
                    , userUsername      = Nothing
                    , userEmail         = Nothing
                    , userVerifiedEmail = False
                    , userVerkey        = Nothing
                    }
 
                _ <- insert $ Ident (credsIdent creds) uid
                return $ Just uid

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins = [ authOpenId 
                  , authFacebook "206687389350404" "9d30284c6cb99ff2c7cbc4e5f8ae53e0" []
                  ]

    authHttpManager = httpManager

    loginHandler = defaultLayout $ do
        setTitle "Login"
        addWidget $(widgetFile "login")

-- Sends off your mail. Requires sendmail in production!
deliver :: Renters -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Renters FormMessage where
    renderMessage _ _ = defaultFormMessage
