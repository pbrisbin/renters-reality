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

import Database.Persist.GenericSql
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
