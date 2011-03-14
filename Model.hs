{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Model where

import Yesod
import BadLandlords
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
        formatted       String
        landlord        LandlordId Eq

    Complaint
        createdDate     UTCTime Desc
        moveIn          UTCTime
        moveOut         UTCTime
        content         String
        authorName      String Eq
        authorEmail     String Eq
        landlord        LandlordId Eq
        property        PropertyId Eq

    Comment
        createdDate     UTCTime Desc
        authorName      String Eq
        authorEmail     String Eq
        content         String
        complaint       ComplaintId Eq

    Tag
        name            String Eq
        complaint       ComplaintId Eq
    |]

-- | An address as might be entered into a form by the user
data Addr = Addr
    { addrOne :: String -- ^ 112 Main St
    , addrTwo :: String -- ^ Apt 2
    , city    :: String -- ^ Cambridge
    , state   :: String -- ^ MA
    , zip     :: String -- ^ 02139
    }

-- | Search criteria for Properties or complaints
data Criteria = CriteriaLandlord  String
              | CriteriaZip       String
              | CriteriaCityState (String,String)
              | CriteriaAddress   Addr

findOrCreateLandlord :: String -> Handler Landlord
findOrCreateLandlord = undefined

propertySearch :: Criteria -> Handler [Property]
propertySearch = undefined

complaintSearch :: Criteria -> Handler [Complaint]
complaintSearch = undefined
