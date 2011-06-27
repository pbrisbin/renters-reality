{-# OPTIONS -fno-warn-orphans      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Controller
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Controller (withServer) where

import Renters
import Model
import Handlers

import Yesod
import Yesod.Comments.Storage
import Yesod.Helpers.Auth
import Yesod.Helpers.Static

import Control.Monad (forM)
import Database.Persist.GenericSql
import qualified Data.Map as M
import qualified Settings

-- | Instantiate the Yesod route types
mkYesodDispatch "Renters" resourcesRenters

-- | Create a Wai App of the site
withServer :: (Application -> IO a) -> IO a
withServer f = Settings.withConnectionPool $ \p -> do
    runSqlPool (runMigration doMigration) p
    runSqlPool (runMigration migrateComments) p
    f =<< toWaiApp (Renters s loadDocuments p)
    where
        s = static Settings.staticDir

        loadDocuments :: Handler [Document]
        loadDocuments = do
            users      <- return . M.fromList =<< runDB (selectList [] [UserUsernameAsc] 0 0)
            landlords  <- return . M.fromList =<< runDB (selectList [] [LandlordNameAsc] 0 0)
            reviews    <- runDB $ selectList [] [ReviewCreatedDateDesc] 0 0

            docs <- forM reviews $ \(k,v) -> do
                let u = M.lookup (reviewReviewer v) users
                let l = M.lookup (reviewLandlord v) landlords

                return $ case (u, l) of
                    (Just u', Just l') -> [ Document k v l' u' ]
                    _                  -> []

            return $ concat docs
