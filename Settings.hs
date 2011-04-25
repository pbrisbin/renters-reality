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

import Database.Persist.Sqlite
import qualified Yesod
import qualified Data.Text as T

#define DEBUG

#ifdef DEBUG
import System.IO
debug :: Yesod.Yesod m => String -> Yesod.GHandler s m ()
debug = Yesod.liftIO . hPutStrLn stderr
#endif

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

dataBase :: T.Text
dataBase = "db.s3db"

withConnectionPool :: Yesod.MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool dataBase 10
