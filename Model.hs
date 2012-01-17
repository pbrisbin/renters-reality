module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi

import Yesod.Goodies
import Data.Ord  (comparing)
import Data.Time (UTCTime(..))
import qualified Data.Text as T

data Grade = Aplus | A | Aminus
           | Bplus | B | Bminus
           | Cplus | C | Cminus
           | Dplus | D | Dminus
           | F deriving (Eq, Ord, Read, Show)

derivePersistField "Grade"

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlMkSettings, mkMigrate "migrateAll"]
    $(persistFile upperCaseSettings "config/models")

data Document = Document
    { reviewId :: ReviewId
    , review   :: Review
    , landlord :: Landlord
    , user     :: User
    }

showName :: User -> Text
showName (User _         (Just un) _ _ _) = shorten 40 un
showName (User (Just fn) _         _ _ _) = shorten 40 fn
showName _                                = "anonymous"


docsByLandlordId :: LandlordId -> [Document] -> [Document]
docsByLandlordId lid = filter ((== lid) . reviewLandlord . review)

docByReviewId:: ReviewId -> [Document] -> Maybe Document
docByReviewId rid docs =
    case filter ((== rid) . reviewId) docs of
        []    -> Nothing
        (x:_) -> Just x

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

prettyGrade :: Grade -> Text
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
