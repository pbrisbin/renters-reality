{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , setTitle
    , staticDir
    , staticRoot
    , withConnectionPool
    ) where

import Database.Persist.Postgresql
import qualified Yesod
import qualified Data.Text as T

approot :: T.Text
#ifdef PROD
approot = "http://rentersreality.com"
#else
approot = "http://localhost:3000"
#endif

staticDir :: String
staticDir = "static"

staticRoot :: String
staticRoot = T.unpack approot ++ "/static"

setTitle :: (Yesod.Yesod m) => String -> Yesod.GWidget s m ()
setTitle s = Yesod.setTitle . Yesod.toHtml $ "Renters' reality | " ++ s

connStr :: T.Text
connStr = "user=renters password=reality host=localhost port=5432 dbname=renters"

connCount :: Int
connCount = 100

withConnectionPool :: Yesod.MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withPostgresqlPool connStr connCount
