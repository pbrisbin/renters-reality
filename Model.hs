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
import Database.Persist.Base
import Data.Ord  (comparing)
import Data.Time (UTCTime(..))

import qualified Data.Text as T

data Grade = Aplus | A | Aminus
           | Bplus | B | Bminus
           | Cplus | C | Cminus
           | Dplus | D | Dminus
           | F deriving (Eq, Ord, Read, Show)

derivePersistField "Grade"

share [mkPersist, mkMigrate "doMigration"] $(persistFile "config/models")

data Document = Document
    { reviewId :: ReviewId
    , review   :: Review
    , landlord :: Landlord
    , user     :: User
    }

-- | to support "landlord: foo" and "address: bar" type searches, we'll 
--   create some newtypes that wrap document, then make instances of 
--   Search for them which only search the piece defined. Helpers.Search 
--   just needs to wrap the data before calling search if/when it sees 
--   the prefix
newtype LDoc = Land Document -- ^ search just landlord
newtype ADoc = Addr Document -- ^ search just addresses

-- | To search a "document" as text is to search it's landlord's name 
--   and the single-string address info
instance TextSearch Document where
    toText d@(Document _ _ l _) = landlordName l `append` formatAddress d

        where
            append :: T.Text -> T.Text -> T.Text
            a `append` b = a `T.append` " " `T.append` b

instance TextSearch LDoc where
    toText (Land (Document _ _ l _)) = landlordName l

instance TextSearch ADoc where
    toText (Addr d) = formatAddress d

-- | Search by keyword and lend preference to more recent reviews
instance Search Document where
    preference = comparing (reviewCreatedDate . review . searchResult)
    match      = keywordMatch

instance Search LDoc where
    preference = comparing (reviewCreatedDate . review . (\(Land d) -> d) . searchResult)
    match      = keywordMatch

instance Search ADoc where
    preference = comparing (reviewCreatedDate . review . (\(Addr d) -> d) . searchResult)
    match      = keywordMatch

showName :: User -> T.Text
showName (User _         (Just un) _ _ _) = shorten 40 un
showName (User (Just fn) _         _ _ _) = shorten 40 fn
showName _                                = "anonymous"

formatAddress :: Document -> T.Text
formatAddress (Document _ r _ _) = T.map go . T.filter (/= '\r') . unTextarea $ reviewAddress r

    where
        go :: Char -> Char
        go '\n' = ' '
        go x    = x

docsByLandlord :: LandlordId -> [Document] -> [Document]
docsByLandlord lid = filter ((lEq lid) . reviewLandlord . review)

    where
        lEq :: LandlordId -> LandlordId -> Bool
        lEq a b = a == b || go (unLandlordId a) (unLandlordId b)

        go (PersistText  t) (PersistInt64 i) = t == (T.pack $ show i)
        go (PersistInt64 i) (PersistText  t) = t == (T.pack $ show i)
        go _                _                = False

docByReviewId:: ReviewId -> [Document] -> Maybe Document
docByReviewId rid docs =
    case filter ((rEq rid) . reviewId) docs of
        []    -> Nothing
        (x:_) -> Just x

    where
        rEq :: ReviewId -> ReviewId -> Bool
        rEq a b = a == b || go (unReviewId a) (unReviewId b)

        go (PersistText  t) (PersistInt64 i) = t == (T.pack $ show i)
        go (PersistInt64 i) (PersistText  t) = t == (T.pack $ show i)
        go _                _                = False

gpa :: [Grade] -> Double
gpa = mean . map toNumeric

    where
        -- http://cgi.cse.unsw.edu.au/~dons/blog/2008/05/16#fast
        mean :: [Double] -> Double
        mean = go 0 0
            where
                go :: Double -> Int -> [Double] -> Double
                go s l []     = s / fromIntegral l
                go s l (x:xs) = go (s+x) (l+1) xs

        toNumeric :: Grade -> Double
        toNumeric Aplus  = 4.5
        toNumeric A      = 4.0
        toNumeric Aminus = 3.75
        toNumeric Bplus  = 3.5
        toNumeric B      = 3.0
        toNumeric Bminus = 2.75
        toNumeric Cplus  = 2.5
        toNumeric C      = 2.0
        toNumeric Cminus = 1.75
        toNumeric Dplus  = 1.5
        toNumeric D      = 1.0
        toNumeric Dminus = 0.75
        toNumeric F      = 0.0

prettyGrade :: Grade -> T.Text
prettyGrade Aplus  = "A+"
prettyGrade A      = "A"
prettyGrade Aminus = "A-"
prettyGrade Bplus  = "B+"
prettyGrade B      = "B"
prettyGrade Bminus = "B-"
prettyGrade Cplus  = "C+"
prettyGrade C      = "C"
prettyGrade Cminus = "C-"
prettyGrade Dplus  = "D+"
prettyGrade D      = "D"
prettyGrade Dminus = "D-"
prettyGrade F      = "F"
