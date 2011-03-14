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
    ) where

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
