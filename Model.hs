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

    Complaint
        createdDate     UTCTime Desc
        content         String
        moveIn          UTCTime
        moveOut         UTCTime
        complainer      ComplainerId Eq
        landlord        LandlordId   Eq
        property        PropertyId   Eq

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

-- | An address as might be entered into a form by the user, only
--   zipcode is mandatory
data Addr = Addr
    { addrOne   :: Maybe String -- ^ 112 Main St
    , addrTwo   :: Maybe String -- ^ Apt 2
    , addrCity  :: Maybe String -- ^ Cambridge
    , addrState :: Maybe String -- ^ MA
    , addrZip   :: String       -- ^ 02139
    }

-- | The two ways we can search complaints: by landlord or (partial)
--   address
data Search = LandlordSearch Landlord | PropertySearch Addr

findOrCreateLandlord :: Landlord -> Handler LandlordId
findOrCreateLandlord landlord = do
    result <- runDB $ insertBy landlord
    case result of
        Left (k, v) -> return k
        Right k     -> return k

findOrCreateProperty :: Addr -> Handler PropertyId
findOrCreateProperty addr = do
    let property = Property
            { propertyAddrOne  = fromMaybe "" $ addrOne   addr
            , propertyAddrTwo  = fromMaybe "" $ addrTwo   addr
            , propertyCity     = fromMaybe "" $ addrCity  addr
            , propertyState    = fromMaybe "" $ addrState addr
            , propertyZip      = addrZip addr
            }

    result <- runDB $ insertBy property
    case result of
        Left (k, v) -> return k
        Right k     -> return k

complaintsBySearch :: Search -> Handler [Complaint]
complaintsBySearch (LandlordSearch landlord) = do
    key <- findOrCreateLandlord landlord
    return . map snd =<< runDB (selectList [ComplaintLandlordEq key] [ComplaintCreatedDateDesc] 0 0)

complaintsBySearch (PropertySearch addr) = do
    key <- findOrCreateProperty addr
    return . map snd =<< runDB (selectList [ComplaintPropertyEq key] [ComplaintCreatedDateDesc] 0 0)
