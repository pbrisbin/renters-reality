{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withRenters
    , withDevelAppPort
    ) where

import Foundation
import Settings
import Yesod.Static
import Yesod.Auth
import Yesod.Comments.Storage
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger)
import Control.Monad (forM)
import Data.Dynamic (Dynamic, toDyn)
import qualified Database.Persist.Base
import Database.Persist.GenericSql (runMigration)
import qualified Data.Map as M

import Handler.Root
import Handler.Legal
import Handler.Search
import Handler.New
import Handler.Edit
import Handler.Profile
import Handler.Reviews
import Handler.Landlords
import Handler.Feed

mkYesodDispatch "Renters" resourcesRenters

withRenters :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withRenters conf logger f = do
#ifdef PRODUCTION
    s <- static Settings.staticDir
#else
    s <- staticDevel Settings.staticDir
#endif
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            $ either error return . Database.Persist.Base.loadConfig
    Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        Database.Persist.Base.runPool dbconf (runMigration migrateAll     ) p
        Database.Persist.Base.runPool dbconf (runMigration migrateComments) p
        defaultRunner f $ Renters conf logger s p loadDocuments

    where
        loadDocuments :: Handler [Document]
        loadDocuments = do
            users     <- return . M.fromList =<< runDB (selectList [] [Asc  UserUsername     ])
            landlords <- return . M.fromList =<< runDB (selectList [] [Asc  LandlordName     ])
            reviews   <-                         runDB (selectList [] [Desc ReviewCreatedDate])

            docs <- forM reviews $ \(k,v) -> do
                let u = M.lookup (reviewReviewer v) users
                let l = M.lookup (reviewLandlord v) landlords

                return $ case (u, l) of
                    (Just u', Just l') -> [ Document k v l' u' ]
                    _                  -> []

            return $ concat docs

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withRenters
