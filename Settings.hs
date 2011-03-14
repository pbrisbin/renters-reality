{-# LANGUAGE CPP #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Settings
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Settings
    ( approot
    , staticDir
    , staticRoot
    , withConnectionPool
    ) where

import Control.Monad.IO.Peel (MonadPeelIO)
import Database.Persist.Sqlite

approot :: String
#ifdef PROD
approot = "http://badbostonlandlords.com"
#else
approot = "http://pbrisbin.com:8080"
#endif

staticDir :: String
staticDir = "static"

staticRoot :: String
staticRoot = approot ++ "/static"

dataBase :: String
dataBase = "db.s3db"

withConnectionPool :: MonadPeelIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool dataBase 10
