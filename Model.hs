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
import Data.Ord  (comparing)
import Data.Time (UTCTime(..))

import qualified Data.Text as T

data Grade = Aplus | A | Aminus
           | Bplus | B | Bminus
           | Cplus | C | Cminus
           | Dplus | D | Dminus
           | F deriving (Eq, Ord, Read, Show)

derivePersistField "Grade"

share2 mkPersist (mkMigrate "doMigration") [persist|
    Landlord
        name T.Text Eq Asc
        UniqueLandlord name

    Review
        createdDate UTCTime Desc
        reviewer    UserId     Eq
        landlord    LandlordId Eq
        grade       Grade      Eq Asc Desc
        ipAddress   T.Text
        address     Textarea
        timeframe   T.Text
        content     Markdown

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
    , user     :: User
    }

instance TextSearch Document where
    toText (Document _ r l _) = landlordName l `append` (T.pack . show $ reviewAddress r)

        where
            append :: T.Text -> T.Text -> T.Text
            a `append` b = a `T.append` " " `T.append` b

instance Search Document where
    preference = comparing (reviewCreatedDate . review . searchResult)
    match      = keywordMatch

showName :: User -> T.Text
showName (User _         (Just un) _ _ _) = shorten 40 un
showName (User (Just fn) _         _ _ _) = shorten 40 fn
showName _                                = "anonymous"
