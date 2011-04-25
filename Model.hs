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
import Yesod.Comments.Markdown
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
        name T.Text Eq Asc
        UniqueLandlord name

    Property
        addrOne T.Text Eq Asc
        addrTwo T.Text Eq Asc
        city    T.Text Eq Asc
        state   T.Text Eq Asc
        zip     T.Text Eq Asc
        UniqueProperty addrOne addrTwo city state zip

    Ownership
        property PropertyId Eq
        landlord LandlordId Eq
        UniqueOwnership property landlord

    Review
        createdDate UTCTime Desc
        ipAddress   T.Text
        type        ReviewType Eq
        content     Markdown
        timeframe   T.Text
        reviewer    UserId     Eq
        landlord    LandlordId Eq
        property    PropertyId Eq

    User
        fullname      T.Text Maybe Update
        username      T.Text Maybe Update Asc
        email         T.Text Maybe Update
        verifiedEmail Bool default=false Eq Update
        verkey        T.Text Maybe Update

    Ident
        ident T.Text Asc
        user  UserId Eq
        UniqueIdent ident
    |]

data Document = Document
    { reviewId :: ReviewId
    , review   :: Review
    , landlord :: Landlord
    , property :: Property
    , user     :: User
    } deriving Show

-- search helpers

keyWordMatch :: T.Text -> T.Text -> Bool
keyWordMatch a b = T.words a `anyIn` T.words b

looseMatch :: T.Text -> T.Text -> Bool
looseMatch a b = fix a `T.isInfixOf` fix b

anyIn :: Eq a => [a] -> [a] -> Bool
anyIn []     _  = False
anyIn _      [] = False
anyIn (x:xs) ys = x `elem` ys || xs `anyIn` ys

fix :: T.Text -> T.Text
fix = T.toLower . T.filter (`notElem` [',', '.', '#'])

-- formatting helpers

formatProperty :: Property -> T.Text
formatProperty p = T.intercalate (T.pack ", ")
                 . map T.strip . filter (not . T.null)
                 $ [ propertyAddrOne p
                   , propertyAddrTwo p
                   , propertyCity    p
                   , propertyState   p
                   , propertyZip     p
                   ]

showName :: User -> T.Text
showName (User _         (Just un) _ _ _) = shorten 40 un
showName (User (Just fn) _         _ _ _) = shorten 40 fn
showName _                                = T.pack "anonymous"

shorten :: Int -> T.Text -> T.Text
shorten n s = if T.length s > n then T.append "..." $ T.take (n - 3) s else s

-- | Same for strings
shorten' :: Int -> String -> String
shorten' n s = if length s > n then take (n - 3) s ++ "..." else s
