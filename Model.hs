module Model where

import Prelude
import Yesod hiding (object)
import Database.Persist.Quasi
import Data.Text (Text)
import Data.Typeable (Typeable)

import Data.Time      (UTCTime(..))
import Yesod.Markdown (Markdown(..))

import Helpers.Grade (Grade(..))

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings "config/models")

-- instance ToJSON Grade where
--     toJSON Aplus   = String "A+"
--     toJSON Aminus  = String "A-"
--     toJSON Bplus   = String "B+"
--     toJSON Bminus  = String "B-"
--     toJSON Cplus   = String "C+"
--     toJSON Cminus  = String "C-"
--     toJSON Dplus   = String "D+"
--     toJSON Dminus  = String "D-"
--     toJSON g       = String . T.pack $ show g

-- instance ToJSON (Entity User) where
--     toJSON (Entity uid u) = object [ "link"     .= [st|/users/#{toPathPiece uid}/|]
--                                    , "username" .= (case (userFullname u, userUsername u) of
--                                                         (Just fn, _      ) -> fn
--                                                         (_,       Just un) -> un
--                                                         _                  -> "anoymous")
--                                    ]

-- instance ToJSON (Entity Landlord) where
--     toJSON (Entity lid l) = object [ "link" .= [st|/landlords/#{toPathPiece lid}/|]
--                                    , "name" .= (landlordName l)
--                                    ]

-- instance ToJSON (Entity Review) where
--     toJSON (Entity rid r) = object [ "link"   .= [st|/reviews/#{toPathPiece rid}/|]
--                                    , "grade"  .= (reviewGrade r)
--                                    , "content".= (unMarkdown $  reviewContent r)
--                                    ]
