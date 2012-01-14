module Helpers.Model (findOrCreate) where

import Prelude
import Yesod
import Database.Persist.Store (Entity(..))

findOrCreate :: (YesodPersist m,
                 PersistUnique (YesodPersistBackend m) (GHandler s m),
                 PersistStore (YesodPersistBackend m) (GHandler s m),
                 PersistEntity v)
             => v -> GHandler s m (Key (YesodPersistBackend m) v)
findOrCreate v = return . either entityKey id =<< runDB (insertBy v)
