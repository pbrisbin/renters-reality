{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import BadLandlords

import Control.Monad               (liftM)
import Data.Time                   (UTCTime(..))
import Data.List                   (intercalate)
import Data.Maybe                  (fromMaybe)
import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

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

    Complainer
        name            String
        email           String
        ipAddress       String
        UniqueComplainer name email ipAddress

    Complaint
        reference       Int Eq Desc
        createdDate     UTCTime Desc
        content         String
        complainer      ComplainerId Eq
        landlord        LandlordId   Eq
        property        PropertyId   Eq
        UniqueComplaint reference

    Commenter
        name            String
        email           String
        ipAddress       String

    Comment
        createdDate     UTCTime Desc
        content         String
        complaint       ComplaintId Eq
        commenter       CommenterId Eq
    |]

-- | Find or create the entity, returning it's key in both cases
findOrCreate :: PersistEntity a => a -> Handler (Key a)
findOrCreate v = do
    result <- runDB $ insertBy v
    case result of
        Left (k,v') -> return k
        Right k     -> return k

-- | Find an entity by it's key
findByKey :: PersistEntity a => Key a -> Handler (Maybe a)
findByKey key = runDB $ get key

-- | Get the next available complaint ref
newRef :: Handler Int
newRef = do
    result <- runDB $ selectList [] [ComplaintReferenceDesc] 1 0
    return . go $ map (complaintReference . snd) result
    where
        go []  = 0
        go [x] = x + 1

complaintsByLandlord :: Landlord -> Handler [Complaint]
complaintsByLandlord landlord = do
    key <- findOrCreate landlord
    return . map snd =<< runDB (selectList [ComplaintLandlordEq key] [ComplaintCreatedDateDesc] 0 0)

complaintsByProperty :: [Property] -> Handler [Complaint]
complaintsByProperty properties = liftM concat $ mapM go properties 
    where
        go :: Property -> Handler [Complaint]
        go property = do
            key <- findOrCreate property
            return . map snd =<< runDB (selectList [ComplaintPropertyEq key] [ComplaintCreatedDateDesc] 0 0)
