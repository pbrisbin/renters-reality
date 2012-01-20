module Model where

import Prelude
import Yesod
import Yesod.Markdown
import Data.Text (Text)

import Data.Time (UTCTime(..))

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
