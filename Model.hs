{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
--
-- Most of the user/profile stuff was taken from Haskellers.com:
-- https://github.com/snoyberg/haskellers/
--
module Model where

import Yesod
import Data.Char                   (toLower, isSpace)
import Data.List                   (isInfixOf, intercalate)
import Data.Time                   (UTCTime(..))
import Database.Persist.TH         (derivePersistField, share2)
import Database.Persist.GenericSql (mkMigrate)

-- | Reviews can be good or bad
data ReviewType = Positive | Negative 
    deriving (Show,Read,Eq)

instance SinglePiece ReviewType where
    toSinglePiece Positive = "positive"
    toSinglePiece Negative = "negative"

    fromSinglePiece "positive" = Right Positive
    fromSinglePiece "negative" = Right Negative
    fromSinglePiece _          = Left "invalid review type"

derivePersistField "ReviewType"

-- | Matchable against a string, supports searching
class PersistEntity a => Matchable a where
    match :: String -> (Key a, a) -> Bool

-- | Implement a searchable database value by providing your own result 
--   set based on the search term
class PersistEntity a => Searchable a where
    search :: (YesodPersist m, PersistBackend (YesodDB m (GGHandler s m IO))) 
           => String -- ^ search term
           -> GHandler s m [(Key a, a)]

share2 mkPersist (mkMigrate "doMigration") [persist|
    Landlord
        name String Eq Asc
        UniqueLandlord name

    Property
        addrOne String Eq Asc
        addrTwo String Eq Asc
        city    String Eq Asc
        state   String Eq Asc
        zip     String Eq Asc
        UniqueProperty addrOne addrTwo city state zip

    Ownership
        property PropertyId Eq
        landlord LandlordId Eq
        UniqueOwnership property landlord

    Review
        createdDate UTCTime Desc
        ipAddress   String
        type        ReviewType Eq
        content     String
        timeframe   String
        reviewer    UserId     Eq
        landlord    LandlordId Eq
        property    PropertyId Eq

    User
        fullname      String Maybe Update
        username      String Maybe Update
        email         String Maybe Update
        verifiedEmail Bool default=false Eq Update
        verkey        String Maybe Update

    Ident
        ident String Asc
        user  UserId Eq
        UniqueIdent ident
    |]

instance Matchable Landlord where match s (_,v) = looseMatch s (landlordName   v)
instance Matchable Property where match s (_,v) = looseMatch s (formatProperty v)

instance Searchable Landlord where
    search s = return . filter (match s) =<< runDB (selectList [] [ LandlordNameAsc ] 0 0)

instance Searchable Property where
    search s = return . filter (match s) =<< runDB (selectList [] [ PropertyZipAsc
                                                                  , PropertyStateAsc
                                                                  , PropertyCityAsc
                                                                  , PropertyAddrTwoAsc
                                                                  , PropertyAddrOneAsc
                                                                  ] 0 0)

looseMatch :: String -> String -> Bool
looseMatch a b = fix a `isInfixOf` fix b
    where
        fix = strip . map toLower

        -- remove some punctuation before comparing
        strip []       = []
        strip (',':xs) = strip xs
        strip ('.':xs) = strip xs
        strip (x:xs)   = x : strip xs

formatProperty :: Property -> String
formatProperty p = intercalate ", "
                 . map trim
                 . filter (not . null)
                 $ [ propertyAddrOne p
                   , propertyAddrTwo p
                   , propertyCity    p
                   , propertyState   p
                   , propertyZip     p
                   ]
    where
        trim = f . f
        f    = reverse . dropWhile isSpace

showName :: User -> String
showName (User _         (Just un) _ _ _) = shorten 50 40 un
showName (User (Just fn) _         _ _ _) = shorten 50 40 fn
showName _                                = "anonymous"

shorten :: Int -> Int -> String -> String
shorten m n s = if length s > m then take n s ++ "..." else s
