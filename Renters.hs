{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Renters
    ( Renters(..)
    , RentersRoute(..)
    , resourcesRenters
    , Handler
    , Widget
    , module Yesod
    , module Settings
    , module Model
    ) where

import Model
import Yesod hiding (setTitle)
import Yesod.Helpers.RssFeed
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Auth.Facebook
import Yesod.Comments hiding (userName, userEmail)
import Yesod.Comments.Storage
import Data.Maybe (fromMaybe)
import Database.Persist.GenericSql

import Settings ( setTitle
                , staticLink
                , hamletFile
                , cassiusFile
                , luciusFile
                , juliusFile
                , widgetFile
                )

import qualified Settings

data Renters = Renters
    { siteDocs  :: Handler [Document]
    , connPool  :: ConnectionPool 
    }

type Handler = GHandler Renters Renters
type Widget  = GWidget  Renters Renters

mkYesodData "Renters" $(parseRoutesFile "config/routes")

instance Yesod Renters where 
    approot   _ = Settings.approot
    authRoute _ = Just $ AuthR LoginR

    defaultLayout widget = do
        (t,h) <- breadcrumbs
        mmesg <- getMessage
        mauth <- maybeAuth
        pc    <- widgetToPageContent $ do
            rssLink FeedR "rss feed"
            widget
        hamletToRepHtml $(hamletFile "default-layout")

instance YesodPersist Renters where
    type YesodDB Renters = SqlPersist
    runDB db = liftIOHandler $ fmap connPool getYesod >>= runSqlPool db

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
    deleteComment    = deleteCommentPersist
    loadComments     = loadCommentsPersist
    displayUser  uid = return .                maybe ""      showName  =<< runDB (get uid)
    displayEmail uid = return . fromMaybe "" . maybe Nothing userEmail =<< runDB (get uid)

instance YesodAuth Renters where
    type AuthId Renters = UserId

    loginDest _  = ProfileR
    logoutDest _ = RootR

    getAuthId creds = do
        muid <- maybeAuth
        x    <- runDB $ getBy $ UniqueIdent $ credsIdent creds
        case (x, muid) of
            (Just (_, i), Nothing      ) -> return $ Just $ identUser i
            (Nothing    , Just (uid, _)) -> do
                _ <- runDB $ insert $ Ident (credsIdent creds) uid
                return $ Just uid

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

            (Just _, Just _) -> do -- this shouldn't happen
                setMessage "That identifier is already attached to an account."
                redirect RedirectTemporary ProfileR

    authPlugins = [ authOpenId 
                  , authFacebook "206687389350404" "9d30284c6cb99ff2c7cbc4e5f8ae53e0" []
                  ]

    loginHandler = defaultLayout $ do
        setTitle "Login"
        addWidget $(widgetFile "login")
