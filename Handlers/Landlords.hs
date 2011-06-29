{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Handlers.Landlords (getLandlordsR) where

import Renters
import Model
import Yesod
import Database.Persist.Base
import Yesod.Comments
import Yesod.Goodies.Markdown
import Yesod.Goodies.Time
import qualified Data.Text as T
import qualified Settings

getLandlordsR :: Key Landlord -> Handler RepHtml
getLandlordsR lid = do
    docs <- siteDocs =<< getYesod
    defaultLayout $ do
        Settings.setTitle "View landlord"
        [hamlet| Landlord page |]

-- | Somehow related to the persistent upgrade, keys are stored as 
--   PersistInt64 Int64 but when used as a singlePiece they come in as 
--   PersistText Text. This custom eq will ensure that the reviews are 
--   still found
lEq :: LandlordId -> LandlordId -> Bool
lEq a b = a == b || go (unLandlordId a) (unLandlordId b)

    where
        go (PersistText  t) (PersistInt64 i) = t == (T.pack $ show i)
        go (PersistInt64 i) (PersistText  t) = t == (T.pack $ show i)
        go _                _                = False
