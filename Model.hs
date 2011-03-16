{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import BadLandlords
import Data.Time                   (UTCTime(..))
import Data.List                   (intercalate)
import Data.Maybe                  (fromMaybe)
import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

share2 mkPersist (mkMigrate "doMigration") [$persist|
    Landlord
        name String Eq
        UniqueLandlord name

    Property
        addrOne         String
        addrTwo         String
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

findOrCreate :: PersistEntity a => a -> Handler (Key a)
findOrCreate v = do
    result <- runDB $ insertBy v
    case result of
        Left (k,v') -> return k
        Right k     -> return k

-- | Get the next available reference for a complaint
newRef :: Handler Int
newRef = do
    result <- runDB $ selectList [] [ComplaintReferenceDesc] 1 0
    return . go $ map (complaintReference . snd) result
    where
        go []  = 0
        go [x] = x + 1
