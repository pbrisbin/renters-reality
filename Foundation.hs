{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
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
import qualified Settings
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (setTitle, widgetFile)
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Data.Maybe (fromMaybe)

data Renters = Renters
    { settings    :: AppConfig DefaultEnv ()
    , getLogger   :: Logger
    , getStatic   :: Static
    , connPool    :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig
    , httpManager :: Manager
    , siteDocs    :: GHandler Renters Renters [Document]
    }

mkMessage "Renters" "messages" "en"

mkYesodData "Renters" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Renters Renters (FormResult x, Widget)

instance Yesod Renters where
    approot = appRoot . settings

    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        y     <- getYesod
        mmsg  <- getMessage
        mauth <- maybeAuth
        pc    <- widgetToPageContent $ do
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

instance YesodPersist Renters where
    type YesodPersistBackend Renters = SqlPersist
    runDB f = fmap connPool getYesod >>= Database.Persist.Store.runPool (undefined :: Settings.PersistConfig) f

instance YesodAuth Renters where
    type AuthId Renters = UserId

    loginDest _  = ProfileR
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

    authPlugins = [ authOpenId 
                  , authFacebook "206687389350404" "9d30284c6cb99ff2c7cbc4e5f8ae53e0" []
                  ]

    authHttpManager = httpManager

    --loginHandler = defaultLayout $ do
        --setTitle "Login"
        --addWidget $(widgetFile "login")

instance RenderMessage Renters FormMessage where
    renderMessage _ _ = defaultFormMessage
