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
