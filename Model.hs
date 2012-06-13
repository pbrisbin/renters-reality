module Model where

import Prelude
import Yesod hiding (object)
import Yesod.Markdown
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson.Types
import Data.Time (UTCTime(..))
import Text.Shakespeare.Text (st)

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
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFile "config/models")

instance ToJSON Grade where
    toJSON Aplus   = String "A+"
    toJSON Aminus  = String "A-"
    toJSON Bplus   = String "B+"
    toJSON Bminus  = String "B-"
    toJSON Cplus   = String "C+"
    toJSON Cminus  = String "C-"
    toJSON Dplus   = String "D+"
    toJSON Dminus  = String "D-"
    toJSON g       = String . T.pack $ show g

instance ToJSON (Entity User) where
    toJSON (Entity uid u) = object [ "link"     .= [st|/users/#{toPathPiece uid}/|]
                                   , "username" .= (case (userFullname u, userUsername u) of
                                                        (Just fn, _      ) -> fn
                                                        (_,       Just un) -> un
                                                        _                  -> "anoymous")
                                   ]

instance ToJSON (Entity Landlord) where
    toJSON (Entity lid l) = object [ "link" .= [st|/landlords/#{toPathPiece lid}/|]
                                   , "name" .= (landlordName l)
                                   ]

instance ToJSON (Entity Review) where
    toJSON (Entity rid r) = object [ "link"   .= [st|/reviews/#{toPathPiece rid}/|]
                                   , "grade"  .= (reviewGrade r)
                                   , "content".= ((\(Markdown s) -> T.pack s) $ reviewContent r)
                                   ]
