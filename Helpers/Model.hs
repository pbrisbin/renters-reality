module Helpers.Model
    ( findOrCreate
    , joinTables
    , joinTables3
    ) where

import Prelude
import Yesod
import Data.Maybe (catMaybes)
import qualified Data.Map as M

findOrCreate :: (YesodPersist m,
                 PersistUnique (YesodPersistBackend m) (GHandler s m),
                 PersistStore (YesodPersistBackend m) (GHandler s m),
                 PersistEntity v)
             => v -> GHandler s m (Key (YesodPersistBackend m) v)
findOrCreate v = return . either entityKey id =<< runDB (insertBy v)

-- |
--
-- My solution to the N+1 problem:
--
-- > runDB $ do
-- >     posts <- selectList [] []
-- >     users <- selectList [] []
-- >
-- >     let records = joinTables postUser posts users
-- >
-- >     forM records $ \(post,user) -> do
-- >         --
-- >         -- ...
-- >         --
--
joinTables :: (a -> Key backend b)
           -> [Entity backend a]
           -> [Entity backend b]
           -> [(Entity backend a, Entity backend b)]
joinTables f as bs = catMaybes $ map (joinRelation f bs) as

    where
        joinRelation :: (a -> Key backend b) -> [Entity backend b] -> Entity backend a -> Maybe (Entity backend a, Entity backend b)
        joinRelation f bs a = fmap (\b -> (a,b)) $ lookupRelation (f $ entityVal a) (toMap bs)

        lookupRelation :: Key backend b -> M.Map (Key backend b) b -> Maybe (Entity backend b)
        lookupRelation k = fmap (\v -> Entity k v) . M.lookup k

        toMap :: [Entity backend a] -> M.Map (Key backend a) a
        toMap = M.fromList . map (\(Entity k v) -> (k,v))

-- | Same but for three tables (making these as I need them).
joinTables3 :: (a -> Key backend b)
            -> (a -> Key backend c)
            -> [Entity backend a]
            -> [Entity backend b]
            -> [Entity backend c]
            -> [(Entity backend a, Entity backend b, Entity backend c)]
joinTables3 = undefined
