{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import Renters

import Control.Monad               (liftM)
import Data.Time                   (UTCTime(..))
import Data.List                   (intercalate)
import Data.Maybe                  (fromMaybe, fromJust)
import Database.Persist.TH         (derivePersistField, share2)
import Database.Persist.GenericSql (mkMigrate)

derivePersistField "ReviewType"

share2 mkPersist (mkMigrate "doMigration") [$persist|
    Landlord
        name String Eq Asc
        UniqueLandlord name

    Property
        addrOne         String Eq
        addrTwo         String Eq
        city            String Eq
        state           String Eq
        zip             String Eq
        UniqueProperty addrOne addrTwo city state zip

    Ownership
        property        PropertyId Eq
        landlord        LandlordId Eq
        UniqueOwnership property landlord

    Reviewer
        name            String
        email           String
        ipAddress       String
        UniqueReviewer name email ipAddress

    Review
        createdDate     UTCTime Desc
        reference       Int Eq Desc
        type            ReviewType Eq
        content         String
        reviewer        ReviewerId Eq
        landlord        LandlordId   Eq
        property        PropertyId   Eq
        UniqueReview reference

    Commenter
        name            String
        email           String
        ipAddress       String

    Comment
        createdDate     UTCTime Desc
        content         String
        review          ReviewId Eq
        commenter       CommenterId Eq
    |]

-- General db helpers:

-- | Find or create an entity, returning its key in both cases
findOrCreate :: PersistEntity a => a -> Handler (Key a)
findOrCreate v = do
    result <- runDB $ insertBy v
    case result of
        Left (k,v') -> return k
        Right k     -> return k

-- | Find an entity by its key
findByKey :: PersistEntity a => Key a -> Handler (Maybe a)
findByKey key = runDB $ get key

-- | Takes a filter type constructor (SqlFooEq) and a Maybe value, if 
--   the value is not Nothing or Just "", then it returns a listed 
--   application of the constructor on the unwrapped value ([SqlFooEq 
--   x]) to be added to a select statement, otherwise the list is 
--   returned empty and that condition is discarded by the caller. It 
--   sounds more complicated than it really is...
--
--   todo: generalize this beyond Maybe String...
--
maybeCriteria :: (String -> t) -> Maybe String -> [t]
maybeCriteria f v = if notNull v then [ f (fromJust v) ] else []
    where
        notNull :: Maybe String -> Bool
        notNull Nothing   = False
        notNull (Just "") = False
        notNull _         = True

-- Site-specific helpers

-- | Get the next available review ref
newRef :: Handler Int
newRef = do
    result <- runDB $ selectList [] [ReviewReferenceDesc] 1 0
    return . go $ map (reviewReference . snd) result
    where
        go []  = 0
        go [x] = x + 1

reviewsByLandlord :: Landlord -> Handler [Review]
reviewsByLandlord landlord = do
    key <- findOrCreate landlord
    return . map snd =<< runDB (selectList [ReviewLandlordEq key] [ReviewCreatedDateDesc] 0 0)

reviewsByProperty :: [Property] -> Handler [Review]
reviewsByProperty properties = liftM concat $ mapM go properties 
    where
        go :: Property -> Handler [Review]
        go property = do
            key <- findOrCreate property
            return . map snd =<< runDB (selectList [ReviewPropertyEq key] [ReviewCreatedDateDesc] 0 0)
