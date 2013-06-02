module Helpers.Model
    ( findOrCreate
    , joinTables
    , joinTables3
    ) where

import Prelude
import Yesod
import Data.Maybe (catMaybes)
import qualified Data.Map as M

findOrCreate :: ( YesodPersist m
                , PersistUnique (YesodPersistBackend m (HandlerT m IO))
                , PersistEntity v
                , PersistMonadBackend (YesodPersistBackend m (HandlerT m IO)) ~ PersistEntityBackend v
                )
                => v -> HandlerT m IO (Key v)
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
joinTables :: (a -> Key b)
           -> [Entity a]
           -> [Entity b]
           -> [(Entity a, Entity b)]
joinTables f as bs = catMaybes . for as $ \a -> fmap (\b -> (a,b)) $ lookupRelation f a bs

joinTables3 :: (a -> Key b)
            -> (a -> Key c)
            -> [Entity a]
            -> [Entity b]
            -> [Entity c]
            -> [(Entity a, Entity b, Entity c)]
joinTables3 f g as bs cs = catMaybes . for as $ \a ->
    case (lookupRelation f a bs, lookupRelation g a cs) of
        (Just b, Just c) -> Just (a,b,c)
        _                -> Nothing

lookupRelation :: (a -> Key b) -> Entity a -> [Entity b] -> Maybe (Entity b)
lookupRelation f a bs = let k  = f $ entityVal a
                            vs = M.fromList $ map (\(Entity k' v) -> (k',v)) bs
                        in fmap (Entity k) $ M.lookup k vs

for ::  [a] -> (a -> b) -> [b]
for xs f = map f xs
