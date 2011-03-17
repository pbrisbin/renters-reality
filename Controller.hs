{-# LANGUAGE MultiParamTypeClasses #-}
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

import Yesod
import Yesod.Helpers.Static
import BadLandlords
import Model

import Handlers.Root
import Handlers.Legal
import Handlers.Search
import Handlers.New
import Handlers.Create
import Handlers.Complaints
import Handlers.Json

import Database.Persist.GenericSql

import qualified Settings

-- | Instantiate the Yesod route types
mkYesodDispatch "BadLandlords" resourcesBadLandlords

-- | Create a Wai App of the site
withServer :: (Application -> IO a) -> IO a
withServer f = Settings.withConnectionPool $ \p -> do
    runSqlPool (runMigration doMigration) p
    f =<< toWaiApp (BadLandlords s p)
    where
        s = static Settings.staticDir
