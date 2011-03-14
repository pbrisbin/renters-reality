{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import Data.Time                   (UTCTime(..))
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
        deriving Eq

    Ownership
        start           UTCTime Ord Eq
        end             UTCTime Ord Eq
        property        PropertyId
        landlord        LandlordId

    Complaint
        createdDate     UTCTime Desc
        moveIn          UTCTime Ord Eq
        moveOut         UTCTime Ord Eq
        authorName      String Eq
        authorEmail     String Eq
        content         String
        landlord        LandlordId
        property        PropertyId

    Comment
        createdDate     UTCTime Desc
        authorName      String Eq
        authorEmail     String Eq
        content         String
        complaint       ComplaintId

    Tag
        name            String Eq
        complaint       ComplaintId
    |]
