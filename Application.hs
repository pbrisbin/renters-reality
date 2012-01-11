{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withRenters
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
import Yesod.Logger (Logger, logBS, flushLogger)
import Network.Wai.Middleware.RequestLogger (logHandleDev)
#else
import Yesod.Logger (Logger)
import Network.Wai.Middleware.RequestLogger (logStdout)
#endif
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (withManager)

import Control.Monad (forM)
import qualified Data.Map as M

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Legal
import Handler.Search
import Handler.Profile
import Handler.Reviews
import Handler.Feed

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Renters" resourcesRenters

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withRenters :: AppConfig DefaultEnv () -> Logger -> (Application -> IO ()) -> IO ()
withRenters conf logger f = withManager $ \manager -> lift $ do
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig
    Database.Persist.Store.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
        let h = Renters conf logger s p manager loadDocuments
        defaultRunner (f . logWare) h
  where
#ifdef DEVELOPMENT
    logWare = logHandleDev (\msg -> logBS logger msg >> flushLogger logger)
#else
    logWare = logStdout
#endif

    loadDocuments :: Handler [Document]
    loadDocuments = do
        return []
        --users     <- return . M.fromList =<< runDB (selectList [] [Asc  UserUsername     ])
        --landlords <- return . M.fromList =<< runDB (selectList [] [Asc  LandlordName     ])
        --reviews   <-                         runDB (selectList [] [Desc ReviewCreatedDate])

        --docs <- forM reviews $ \(k,v) -> do
            --let u = M.lookup (reviewReviewer v) users
            --let l = M.lookup (reviewLandlord v) landlords

            --return $ case (u, l) of
                --(Just u', Just l') -> [ Document k v l' u' ]
                --_                  -> []

        --return $ concat docs

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withRenters
