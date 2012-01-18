{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , withDevelAppPort
    ) where

import Import
import Settings
import Settings.StaticFiles (staticSite)
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Data.Dynamic (Dynamic, toDyn)
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logHandleDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logHandle)
#endif
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManagerIO)

import Control.Monad (forM)
import qualified Data.Map as M
import Database.Persist.Query.GenericSql ()

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Legal
import Handler.Search
import Handler.Completion
import Handler.Profile
import Handler.Review
import Handler.Feed

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Renters" resourcesRenters

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig DefaultEnv () -> Logger -> IO Application
getApplication conf logger = do
    manager <- newManagerIO 10
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    let foundation = Renters conf setLogger s p manager loadDocuments
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logHandleDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger
    logWare = logHandle (logBS setLogger)
#endif

    loadDocuments :: Handler [Document]
    loadDocuments = do
        users     <- return . fromEntities =<< runDB (selectList [] [Asc  UserUsername     ])
        landlords <- return . fromEntities =<< runDB (selectList [] [Asc  LandlordName     ])
        reviews   <-                           runDB (selectList [] [Desc ReviewCreatedDate])

        docs <- forM reviews $ \(Entity k v) -> do
            let u = M.lookup (reviewReviewer v) users
            let l = M.lookup (reviewLandlord v) landlords

            return $ case (u, l) of
                (Just u', Just l') -> [ Document k v l' u' ]
                _                  -> []

        return $ concat docs

    -- turns a list of entities into a map of key-values
    fromEntities :: [Entity backend a] -> M.Map (Key backend a) a
    fromEntities = M.fromList . map (\e -> (entityKey e, entityVal e))

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp loader getApplication
  where
    loader = loadConfig (configSettings Development)
