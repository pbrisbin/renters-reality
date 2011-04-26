{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- Most of the user/profile stuff was taken from Haskellers.com:
-- <https://github.com/snoyberg/haskellers/>
--
module Model where

import Yesod
import Yesod.Comments.Markdown
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
keyWordMatch a b = (T.words $ fix a) `anyIn` (T.words $ fix b)

looseMatch :: T.Text -> T.Text -> Bool
looseMatch a b = fix a `T.isInfixOf` fix b

allIn :: Eq a => [a] -> [a] -> Bool
allIn []     _  = False
allIn _      [] = False
allIn (x:xs) ys = x `elem` ys && xs `anyIn` ys

anyIn :: Eq a => [a] -> [a] -> Bool
anyIn []     _  = False
anyIn _      [] = False
anyIn (x:xs) ys = x `elem` ys || xs `anyIn` ys

fix :: T.Text -> T.Text
fix = T.toCaseFold . T.filter (`notElem` [',', '.'])

-- formatting helpers

formatProperty :: Property -> T.Text
formatProperty p = T.intercalate ", "
                 . filter (not . T.null)
                 . map T.strip
                 $ [ propertyAddrOne p
                   , propertyAddrTwo p
                   , propertyCity    p
                   , propertyState   p
                   , propertyZip     p
                   ]

showName :: User -> T.Text
showName (User _         (Just un) _ _ _) = shorten 40 un
showName (User (Just fn) _         _ _ _) = shorten 40 fn
showName _                                = "anonymous"

-- | Shorten a variety of string-like types adding ellipsis
class Shorten a where shorten :: Int -> a -> a

instance Shorten String where
    shorten n s = if length s > n then take (n - 3) s ++ "..." else s

instance Shorten T.Text where
    shorten n t = if T.length t > n then T.take (n - 3) t `T.append` "..." else t

instance Shorten Markdown where
    shorten n (Markdown s) = Markdown $ shorten n s
