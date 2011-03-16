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
    { addrOne :: Maybe String -- ^ 112 Main St
    , addrTwo :: Maybe String -- ^ Apt 2
    , city    :: Maybe String -- ^ Cambridge
    , state   :: Maybe String -- ^ MA
    , zip     :: String       -- ^ 02139
    }

-- | Search criteria for properties or complaints
data Criteria = CriteriaLandlord  String
              | CriteriaZip       String
              | CriteriaCityState (String,String)
              | CriteriaAddress   Addr

findOrCreateLandlord :: Landlord -> Handler (Key Landlord)
findOrCreateLandlord landlord = do
    result <- runDB $ insertBy landlord
    case result of
        Left (k, v) -> return k
        Right k     -> return k
