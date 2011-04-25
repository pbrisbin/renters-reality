{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
--
-- Most of the user/profile stuff was taken from Haskellers.com:
-- https://github.com/snoyberg/haskellers/
--
module Model where

import Yesod
import Data.Char (toLower, isSpace)
import Data.List (isInfixOf, intercalate)
import Data.Time (UTCTime(..))

import qualified Data.Text as T

-- | Reviews can be good or bad
data ReviewType = Positive | Negative 
    deriving (Show,Read,Eq)

instance SinglePiece ReviewType where
    toSinglePiece Positive = "positive"
    toSinglePiece Negative = "negative"

    fromSinglePiece "positive" = Just Positive
    fromSinglePiece "negative" = Just Negative
    fromSinglePiece _          = Nothing

derivePersistField "ReviewType"

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
        username      String Maybe Update Asc
        email         String Maybe Update
        verifiedEmail Bool default=false Eq Update
        verkey        String Maybe Update

    Ident
        ident T.Text Asc
        user  UserId Eq
        UniqueIdent ident
    |]

data Document = Document
    { landlord :: Landlord
    , property :: Property
    , review   :: Review
    , user     :: User
    }

kwMatch :: String -> String -> Bool
kwMatch a b = words a `anyIn` words b
    where
        []     `anyIn` _  = False
        (x:xs) `anyIn` ys = x `elem` ys || xs `anyIn` ys

strMatch :: String -> String -> Bool
strMatch a b = fix a `isInfixOf` fix b
    where
        fix = strip . map toLower

        strip []       = []
        strip (',':xs) = ' ' : strip xs
        strip ('.':xs) = ' ' : strip xs
        strip (x:xs)   = x   : strip xs

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
