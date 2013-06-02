module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, hamletFile)
import Model
import Text.Jasmine (minifym)
import System.Log.FastLogger (Logger)

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Network.Gravatar
import Yesod.Comments
import Yesod.Comments.Storage
import Helpers.User
import qualified Data.Text as T

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv ()
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

type DB x = YesodDB App x

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (60 * 60 * 24 * 10) -- 10 days
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        muid <- maybeAuth
        let mgrav = fmap getGravatar muid

        pc <- widgetToPageContent $ do
            -- FIXME combined stuff goes weird in production deploy
            --
            -- $(combineStylesheets 'StaticR
            --     [ css_bootstrap_min_css
            --     , css_bootstrap_responsive_min_css
            --     ])
            -- 
            -- $(combineScripts 'StaticR
            --     [ js_jquery_min_js
            --     , js_bootstrap_min_js
            --     ])

            addStylesheet $ StaticR css_bootstrap_min_css
            addStylesheet $ StaticR css_bootstrap_responsive_min_css

            addScript $ StaticR js_jquery_min_js
            addScript $ StaticR js_jquery_ui_min_js
            addScript $ StaticR js_bootstrap_min_js

            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

        where
            getGravatar :: Entity User -> String
            getGravatar (Entity _ u) = let email = fromMaybe "" $ userEmail u
                                       in  gravatar gravatarOpts email

            gravatarOpts :: GravatarOptions
            gravatarOpts = defaultConfig
                { gSize    = Just $ Size 20
                , gDefault = Just MM
                }

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- FIXME where did loadJsYepnope go?
    --jsLoader _ = BottomOfHeadAsync $ loadJsYepnope $ Right $ StaticR js_modernizr_js

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = ProfileR

    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (Entity _ i) -> do
                updateFromAx (credsExtra creds) $ identUser i
                return $ Just $ identUser i

            Nothing -> do
                uid <- insert User
                    { userFullname      = Nothing
                    , userUsername      = Nothing
                    , userEmail         = Nothing
                    , userVerifiedEmail = False
                    , userVerkey        = Nothing
                    }
 
                _ <- insert $ Ident (credsIdent creds) uid
                updateFromAx (credsExtra creds) uid
                return $ Just uid

        where
            -- updates username/email with values returned by openid
            -- unless values exist there already
            updateFromAx :: [(Text, Text)] -> UserId -> DB ()
            updateFromAx keys uid = maybe (return ()) go =<< get uid

                where
                    go :: User -> DB ()
                    go u = do
                        case (userUsername u, lookup "openid.ext1.value.email" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserUsername =. parseNick val]
                            _                       -> return ()

                        case (userEmail u, lookup "openid.ext1.value.email" keys) of
                            (Nothing, val@(Just _)) -> update uid [UserEmail =. val]
                            _                       -> return ()

                    -- we'll request only email and parse the first
                    -- portion as our username.
                    parseNick :: Maybe Text -> Maybe Text
                    parseNick = fmap (T.takeWhile (/= '@'))

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ authOpenId Claimed
                        -- tested to work with at least google
                        [ ("openid.ax.mode"       , "fetch_request"                         )
                        , ("openid.ax.required"   , "email"                                 )
                        , ("openid.ax.type.email" , "http://schema.openid.net/contact/email")
                        , ("openid.ns.ax"         , "http://openid.net/srv/ax/1.0"          )
                        , ("openid.ns.ax.required", "email"                                 )
                        , ("openid.ui.icon"       , "true"                                  )
                        ] ]

    authHttpManager = httpManager

    loginHandler = lift $ defaultLayout $ do
        setTitle "Login"
        $(widgetFile "login")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler ()
getExtra = fmap (appExtra . settings) getYesod

instance YesodComments App where
    commentStorage = persistStorage

    userDetails uid = do
        mu <- runDB $ get uid
        return $ case mu of
            Just u -> do
                let name = toPathPiece uid

                Just $ case (userUsername u, userEmail u) of
                    (Just uname, Just email) -> UserDetails name uname email
                    (_,          Just email) -> UserDetails name "unknown" email
                    (Just uname, _         ) -> UserDetails name uname ""
                    _                        -> UserDetails name "unknown" ""

            _ -> Nothing

    threadRoute _ = RootR
