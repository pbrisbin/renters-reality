{-# OPTIONS -fno-warn-orphans      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Controller (withServer) where

import Renters

import Handlers.Root
import Handlers.Legal
import Handlers.Search
import Handlers.New
import Handlers.Edit
import Handlers.Profile
import Handlers.Reviews
import Handlers.Landlords
import Handlers.Feed

import Yesod.Helpers.Auth
import Yesod.Comments.Storage
import Control.Monad (forM)
import Database.Persist.GenericSql
import qualified Data.Map as M
import qualified Settings

mkYesodDispatch "Renters" resourcesRenters

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "config/favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)

withServer :: (Application -> IO a) -> IO a
withServer f = Settings.withConnectionPool $ \p -> do
    runSqlPool (runMigration doMigration    ) p
    runSqlPool (runMigration migrateComments) p
    f =<< toWaiApp (Renters p loadDocuments)

    where
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
