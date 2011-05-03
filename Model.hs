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
import Yesod.Goodies.Markdown
import Yesod.Goodies.Search
import Yesod.Goodies.Shorten
import Data.List (intersect)
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
    }

instance Search Document where
    match t d@(Document _ _ l p _) =
        let t' = landlordName l `T.append` formatProperty p
        in  go $ fix t `intersect` fix t'

        where
            go :: [T.Text] -> Maybe (SearchResult Document)
            go [] = Nothing
            go ms = Just $ SearchResult (fromIntegral $ length ms) d

            fix :: T.Text -> [T.Text]
            fix = filter (not . T.null)
                . map T.strip
                . T.words
                . T.toCaseFold
                . T.filter (`notElem` ",.-")

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
