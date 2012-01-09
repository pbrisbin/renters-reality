{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module Foundation
    ( Renters (..)
    , RentersRoute (..)
    , RentersMessage (..)
    , resourcesRenters
    , Handler
    , Widget
    , maybeAuth
    , maybeAuthId
    , requireAuth
    , requireAuthId
    , module Yesod
    , module Settings
    , module Settings.StaticFiles
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    ) where

import Yesod hiding (setTitle, AppConfig(..), withYamlEnvironment)
import Yesod.Static (Static, base64md5, StaticRoute(..))
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Auth.OpenId
import Yesod.Auth.Facebook
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logLazyText)
import Yesod.Comments hiding (userEmail)
import Yesod.Comments.Storage
import qualified Settings
import qualified Database.Persist.Base as Base
import Database.Persist.GenericSql
import Settings (setTitle, widgetFile)
import Model
import Data.Maybe (fromMaybe)
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)

data Renters = Renters
    { settings  :: AppConfig DefaultEnv ()
    , getLogger :: Logger
    , getStatic :: Static
    , connPool  :: Base.PersistConfigPool Settings.PersistConfig
    , siteDocs  :: GHandler Renters Renters [Document]
    }

mkMessage "Renters" "messages" "en"

mkYesodData "Renters" $(parseRoutesFile "config/routes")

instance Yesod Renters where
    approot      = appRoot . settings
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        (t,h) <- breadcrumbs
        mmsg  <- getMessage
        mauth <- maybeAuth
        pc    <- widgetToPageContent $ do
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogMessage loc level msg >>= logLazyText (getLogger y)

    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

instance YesodPersist Renters where
    type YesodPersistBackend Renters = SqlPersist
    runDB f = liftIOHandler
            $ fmap connPool getYesod >>= Base.runPool (undefined :: Settings.PersistConfig) f

instance YesodAuth Renters where
    type AuthId Renters = UserId

    loginDest _  = ProfileR
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueIdent $ credsIdent creds
        case x of
            Just (_, i) -> return $ Just $ identUser i
            Nothing       -> do
                uid <- insert $ User
                    { userFullname      = Nothing
                    , userUsername      = Nothing
                    , userEmail         = Nothing
                    , userVerifiedEmail = False
                    , userVerkey        = Nothing
                    }
 
                _ <- insert $ Ident (credsIdent creds) uid
                return $ Just uid

    authPlugins = [ authOpenId 
                  , authFacebook "206687389350404" "9d30284c6cb99ff2c7cbc4e5f8ae53e0" []
                  ]

    loginHandler = defaultLayout $ do
        setTitle "Login"
        addWidget $(widgetFile "login")

instance RenderMessage Renters FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodBreadcrumbs Renters where
    breadcrumb RootR           = return ("Home"         , Nothing            )
    breadcrumb SearchR         = return ("search"       , Just RootR         )
    breadcrumb (ReviewsR  _)   = return ("view review"  , Just RootR         )
    breadcrumb (LandlordsR _)  = return ("view landlord", Just RootR         )
    breadcrumb NewR            = return ("new"          , Just RootR         )
    breadcrumb (EditR rid)     = return ("edit"         , Just $ ReviewsR rid)
    breadcrumb LegalR          = return ("legal"        , Just RootR         )
    breadcrumb (AuthR _)       = return ("login"        , Just RootR         )
    breadcrumb ProfileR        = return ("profile"      , Just RootR         )
    breadcrumb EditProfileR    = return ("edit"         , Just ProfileR      )
    breadcrumb DeleteProfileR  = return ("delete"       , Just ProfileR      )
    breadcrumb _               = return ("404"          , Just RootR         )

instance YesodComments Renters where
    getComment       = getCommentPersist
    storeComment     = storeCommentPersist
    updateComment    = updateCommentPersist
    deleteComment    = deleteCommentPersist
    loadComments     = loadCommentsPersist
    displayUser  uid = return .                maybe ""      showName  =<< runDB (get uid)
    displayEmail uid = return . fromMaybe "" . maybe Nothing userEmail =<< runDB (get uid)
